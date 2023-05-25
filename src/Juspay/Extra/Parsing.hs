{-
 Copyright 2023-24, Juspay India Pvt Ltd
 This program is free software: you can redistribute it and/or modify it under
 the terms of the GNU Affero General Public License as published by the
 Free Software Foundation, either version 3 of the License, or (at your option)
 any later version. This program is distributed in the hope that it will be
 useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 See the GNU Affero General Public License for more details. You should have
 received a copy of the GNU Affero General Public License along with this
 program. If not, see <https://www.gnu.org/licenses/>.
-}

-- This warning is disabled while this bug remains in the wild:
-- https://github.com/ndmitchell/record-dot-preprocessor/issues/30
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}
{-# OPTIONS_GHC -Wno-unused-foralls #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE ViewPatterns        #-}

module Juspay.Extra.Parsing
  (
  -- * Core definitions
    ParsingErrorType (..)
  , ParsingError (..)
  , Parsed (Failed, Result)
  , Step
  , handleParsed
  , fromParsed
  , toEither

  -- * Parser runners
  , parse
  , parseField

  -- * Helpers for building 'Step's
  , project
  , liftEither
  , liftPure

  -- * Parser combinators
  , around
  , aroundSecret
  , reconcile

  -- * Common parsers collection
  , allAlphaNumericSpecial
  , allAlphaNumeric
  , allElem
  , allNumeric
  , alphaNumSpecial
  , alphaNums
  , numbers
  , defaulting
  , integral
  , length
  , mandated
  , nonEmptyText
  , maybeNonEmptyText
  , lenientMaybeNonEmptyText
  , nonNegative
  , onlyDigits
  , range
  , secret
  , toUTC
  ) where

import           Control.Applicative (liftA2)
import           Control.Arrow (Kleisli (Kleisli), runKleisli)
import           Control.Category (Category ((.)))
import           Data.Char (isDigit)
import           Data.Either (fromRight)
import           Data.Either.Extra (mapRight)
import           Data.Int (Int16, Int32, Int64, Int8)
import           Data.Kind (Type)
import           Data.Maybe (fromMaybe, fromJust)
import           Data.Proxy (Proxy (Proxy))
import           Data.Sequence (Seq (Empty, (:<|)))
import qualified Data.Sequence as Seq
import           Data.Sequence.NonEmpty (NESeq ((:<||)), singleton)
import           Data.Text (Text, pack, unpack)
import qualified Data.Text as T (all, length)
import           Data.Time (LocalTime, UTCTime, localTimeToUTC, utc)
import           Data.Type.Nat
import qualified Data.Vector as V
import           Data.Vector.NonEmpty (NonEmptyVector, uncons, unfoldr1)
import           Juspay.Extra.Secret (Secret, makeSecret, unsafeExtractSecret)
import           Juspay.Extra.NonEmptyText (NonEmptyText, nonEmpty, toText)
import           Juspay.Extra.Text (showT)
import           GHC.Records (HasField, getField)
import           GHC.TypeLits (KnownNat, KnownSymbol, Symbol, symbolVal, natVal)
import qualified GHC.TypeLits as TL (Nat)
import           Optics.Core (preview)
import           Prelude hiding ((.), length)
import           Text.Read (readMaybe)
import           Type.Reflection (Typeable, tyConName, typeRep, typeRepTyCon)
import           Validation (Validation (Failure, Success), validationToEither)


-- | Describes what kind of error we ran into when parsing a particular value.
data ParsingErrorType =
  MandatoryValueMissing {
    typeName :: {-# UNPACK #-} !Text -- ^ Type constructor name for missing value
    } |
  UnexpectedNegative {
    typeName :: {-# UNPACK #-} !Text
    } |
  UnexpectedEmptyText |
  NotAnIntegral {
    typeName :: {-# UNPACK #-} !Text,
    message  :: {-# UNPACK #-} !Text
    } |
  UnexpectedTextValue {
    intendedType :: {-# UNPACK #-} !Text,
    value        :: {-# UNPACK #-} !Text
    } |
  Other {
    message :: {-# UNPACK #-} !Text
    }
  deriving stock (Eq, Show)

-- | Describes the error in full. In particular, when parsing a record field, this
-- tracks the field and record we tried parsing from.
data ParsingError =
  RecordParsingError {
    recordType :: {-# UNPACK #-} !Text,
    fieldName  :: {-# UNPACK #-} !Text,
    errorType  :: !ParsingErrorType
    } |
  OtherParsingError !ParsingErrorType
  deriving stock (Eq, Show)

-- | Represents either (possibly many) errors, or a successful result.
--
-- * 'fmap' applies a pure function to the result, if any.
--
-- * 'pure' produces the given result.
--
-- * '<*>' combines two erroring outcomes (by concatenation), two successes
-- by function application, and a mixture of failure and success by
-- forwarding the failure.
newtype Parsed (a :: Type) = Parsed (Validation (NESeq ParsingError) a)
  deriving (Functor, Applicative)
    via (Validation (NESeq ParsingError))

-- | The 'Foldable' instance is provided only as a prerequisite to 'Traversable';
-- due to its __blind__ treatment of errors (any error, in any number, is treated
-- the same), prefer 'Traversable' functionality if at all possible.
--
-- * 'foldMap' replaces __all__ errors with 'mempty', and applies its function
-- to a success.
deriving via (Validation (NESeq ParsingError)) instance Foldable Parsed

-- | 'traverse' either forwards all errors as-are, not executing the effect at
-- all, or executes the effect (and its transformation) on the result.
instance Traversable Parsed where
  {-# INLINEABLE traverse #-}
  traverse f (Parsed comp) = Parsed <$> traverse f comp

-- | We can use these patterns as if 'Parsed' had the following definition:
--
-- > data Parsed (a :: Type) =
-- >   Failed (NonEmptyVector ParsingError) |
-- >   Result a
--
-- We use this to allow 'Parsed' to be a newtype while still having a pleasant
-- pattern matching syntax, as well as to provide a stable API against changes
-- of internals.
pattern Failed :: NonEmptyVector ParsingError -> Parsed a
pattern Failed errs <- Parsed (Failure (intoNEVector -> errs))
  where
    Failed errs = Parsed . Failure . outOfNEVector $ errs

pattern Result :: a -> Parsed a
pattern Result res <- Parsed (Success res)
  where
    Result res = Parsed . Success $ res

{-# COMPLETE Failed, Result #-}

-- | A handler similar to 'either'. Designed for cases where the 'Parsed' is nested
-- deeply inside other structures.
handleParsed ::
  (NonEmptyVector ParsingError -> b) ->
  (a -> b) ->
  Parsed a ->
  b
handleParsed onFail onSuccess (Parsed comp) =
  either (onFail . intoNEVector) onSuccess . validationToEither $ comp

-- | A handler similar to 'fromMaybe'. Designed for cases where the 'Parsed' is
-- nexted deeply inside other structures.
fromParsed :: a -> Parsed a -> a
fromParsed def (Parsed comp) =
  fromRight def . validationToEither $ comp

-- | Maps a parsing result to 'Either'
toEither :: Parsed b -> Either (NonEmptyVector ParsingError) b
toEither = \case
  Failed errs -> Left errs
  Result x    -> Right x

-- | Represents one parsing step. In particular, we track whether we're parsing a
-- record field or not by way of 'ctx'. You can safely ignore 'ctx' in practice
-- - it exists only to ensure some extra type safety and for better errors.
--
-- * 'fmap' post-processes the result of a 'Step' using a pure function. This is
-- mostly needed as a prerequisite for 'Applicative'.
--
-- * 'pure' produces a 'Step' that ignores its input and produces the given value
-- (without error).
--
-- * '<*>' combines two 'Step's by cloning the input, sending it to both component
-- 'Step's, collecting the results, and combining them with ($). It's more useful
-- when used with 'liftA2', which types as so:
--
-- > liftA2 :: (b -> c -> d) -> Step ctx a b -> Step ctx a c -> Step ctx a d
--
-- * 'id' is a 'Step' which does nothing (essentially, just passes the input
-- through).
--
-- * '>>>' links together two 'Step's in the direction of the tracks.
newtype Step (ctx :: Maybe Symbol) (a :: Type) (b :: Type) =
  Step (Kleisli (Either ParsingErrorType) a b)
  deriving (Functor, Applicative) via (Kleisli (Either ParsingErrorType) a)
  deriving Category via (Kleisli (Either ParsingErrorType))

-- | Run a 'Step' for a standalone (non-record) value.
parse :: a -> Step 'Nothing a b -> Parsed b
parse input (Step comp) = Parsed $ case runKleisli comp input of
  Left err  -> Failure . singleton . OtherParsingError $ err
  Right res -> Success res

-- | Run a 'Step' which draws values from a record. Errors will report the record
-- and field name.
parseField :: forall (field :: Symbol) (a :: Type) (b :: Type) .
  (Typeable a, KnownSymbol field) =>
  a -> Step ('Just field) a b -> Parsed b
parseField input (Step comp) = Parsed $ case runKleisli comp input of
  Left err  -> let recordType = tyName @a
                   fieldName = pack . symbolVal $ Proxy @field
                   errorType = err
                   err' = RecordParsingError{..} in
                Failure . singleton $ err'
  Right res -> Success res

{-------------------------------------------------------------------------------
  Helpers for building 'Step's
-------------------------------------------------------------------------------}

-- | A 'Step' that projects a field out of a record. This Just Works (tm) with
-- record dot syntax enabled, and tracks where the projection came from.
--
-- Formerly 'withField'.
project :: forall (field :: Symbol) (r :: Type) (a :: Type) .
  (HasField field r a) => Step ('Just field) r a
project = Step . Kleisli $ Right . getField @field

-- | Lift an existing function which can fail into a 'Step'.
liftEither :: (a -> Either ParsingErrorType b) -> Step ctx a b
liftEither = Step . Kleisli

-- | Lift a pure function into a 'Step'.
liftPure :: (a -> b) -> Step ctx a b
liftPure f = Step . Kleisli $ pure . f

{-------------------------------------------------------------------------------
  Helpers for transforming 'Step's
-------------------------------------------------------------------------------}

-- | Promote a 'Step' which works on pure values to working on values inside
-- 'Maybe'.
--
-- Formerly 'insideJust'.
around :: Step ctx a b -> Step ctx (Maybe a) (Maybe b)
around (Step comp) = Step . Kleisli . go . runKleisli $ comp
  where
    go ::
      (a -> Either ParsingErrorType b) ->
      Maybe a ->
      Either ParsingErrorType (Maybe b)
    go f = \case
      Nothing -> pure Nothing
      Just x  -> Just <$> f x

-- | Promote a 'Step' which works on pure values to working on values inside
-- 'Secret'.
aroundSecret :: Step ctx a b -> Step ctx (Secret a) (Secret b)
aroundSecret (Step comp) = Step . Kleisli . go . runKleisli $ comp
  where
    go ::
      (a -> Either ParsingErrorType b) ->
      Secret a ->
      Either ParsingErrorType (Secret b)
    go f secret' = makeSecret <$> f (unsafeExtractSecret secret')

-- | A more capable 'liftA2', which allows the /combining function/ to fail in a
-- way that's different to the 'Step's being combined.
reconcile ::
  forall (a :: Type) (b :: Type) (c :: Type) (d :: Type) (ctx :: Maybe Symbol) .
  (b -> c -> Either ParsingErrorType d) ->
  Step ctx a b ->
  Step ctx a c ->
  Step ctx a d
reconcile f (Step comp1) (Step comp2) =
  Step . Kleisli . go . runKleisli $ liftA2 (,) comp1 comp2
    where
      go ::
        (a -> Either ParsingErrorType (b, c)) ->
        a ->
        Either ParsingErrorType d
      go g x = case g x of
        Left err     -> Left err
        Right (l, r) -> f l r

{-------------------------------------------------------------------------------
  Common parsers collection
-------------------------------------------------------------------------------}

-- | A step to check wether all characters belongs to the list specified
allElem :: [Char] -> Step ctx NonEmptyText NonEmptyText
allElem enum = liftEither go
  where
    go :: NonEmptyText -> Either ParsingErrorType NonEmptyText
    go t = if T.all isEnumElem (toText t)
      then Right t
      else Left $ Other $ pack "Only these characters are allowed here: [" <> showT enum <> "]"
    isEnumElem = flip elem enum

-- | A 'Step' to ensure all characters are alpha-numeric-special ones.
allAlphaNumericSpecial :: Step ctx NonEmptyText NonEmptyText
allAlphaNumericSpecial = allElem alphaNumSpecial

-- | A 'Step' to ensure all characters are alpha-numeric ones.
allAlphaNumeric :: Step ctx NonEmptyText NonEmptyText
allAlphaNumeric = allElem alphaNums

-- | A 'Step' to usnure all characters are numbers.
allNumeric :: Step cxt NonEmptyText NonEmptyText
allNumeric = allElem numbers

numbers :: [Char]
numbers = ['0' .. '9']

alphaNums :: [Char]
alphaNums = ['a'..'z'] ++ ['A'..'Z'] ++ numbers

specialChars :: [Char]
specialChars =
  [ '~', '!', '@', '#', '$', '%', '^'
  , '&', '*', '(', ')', '_', '+', '='
  , '-', ',', '.', '/', '<', '>', '?'
  , '[', ']', '\\', '|', ':', ';', '"'
  , '\'', ' '
  ]

alphaNumSpecial :: [Char]
alphaNumSpecial = alphaNums <> specialChars

-- | A 'Step' which either promotes a value out of 'Maybe', or replaces it with
-- the provided default.
--
-- Formerly @extractMaybeWithDefault@.
--
-- >>> toEither $ parse (Nothing :: Maybe Text) (defaulting "def")
-- Right "def"
defaulting :: a -> Step ctx (Maybe a) a
defaulting def = Step . Kleisli $ Right . fromMaybe def

-- | A 'Step' which parses 'Text' into an 'Integral' instance, erroring if this fails.
-- Might require a type annotation for @a@, e.g:
--
-- Formerly @toInt@.
--
-- >>> toEither $ parse ("42" :: Text) (integral @Int64 >>> nonNegative)
-- Right 42
integral :: forall (a :: Type) (ctx :: Maybe Symbol) .
  (Integral a, Bounded a, Typeable a) => Step ctx Text a
integral = liftEither go
  where
    go s = maybe (Left (UnexpectedTextValue (tyName @a) s))
            integerToBounded . readMaybe . unpack $ s

-- Helper function for 'integral'
integerToBounded :: forall a. (Integral a, Bounded a, Typeable a) => Integer -> Either ParsingErrorType a
integerToBounded n
    | n < toInteger (minBound @a) = Left . Other $ "The value is less than minBound of " <>  tyName @a
    | n > toInteger (maxBound @a) = Left . Other $ "The value is greater than maxBound of " <> tyName @a
    | otherwise                   = Right (fromIntegral n)
{-# INLINE integerToBounded #-}
{-# SPECIALISE integerToBounded :: Integer -> Either ParsingErrorType Int #-}
{-# SPECIALISE integerToBounded :: Integer -> Either ParsingErrorType Int8 #-}
{-# SPECIALISE integerToBounded :: Integer -> Either ParsingErrorType Int16 #-}
{-# SPECIALISE integerToBounded :: Integer -> Either ParsingErrorType Int32 #-}
{-# SPECIALISE integerToBounded :: Integer -> Either ParsingErrorType Int64 #-}

-- | 'Step' to ensure 'Text' has a specific length. Consider stripping
-- spaces before using this step.
length :: forall (len :: TL.Nat) (ctx :: Maybe Symbol)
  . (SNatI (FromGHC len), KnownNat len)
  => Step ctx Text (TextByLength (FromGHC len))
length = range @len @len

-- | A 'Step' which ensures that a 'Maybe' value is present, erroring otherwise,
-- and enshrines that by having its output be outside of 'Maybe'.
--
-- Formerly 'extractJust'.
--
-- >>> toEither $ parse (Just "True" :: Maybe Text) (mandated >>> nonEmptyText)
-- Right "True"
mandated :: forall (a :: Type) (ctx :: Maybe Symbol) .
  (Typeable a) => Step ctx (Maybe a) a
mandated = Step . Kleisli $ maybe (Left mkError) Right
  where
    mkError :: ParsingErrorType
    mkError = MandatoryValueMissing (tyName @a)

-- | A 'Step' that ensures a 'Text' is not empty, and enshrines that into the type
-- of its output.
--
-- Formerly 'textNotEmpty'.
nonEmptyText :: forall (ctx :: Maybe Symbol) . Step ctx Text NonEmptyText
nonEmptyText = Step . Kleisli $
  maybe (Left UnexpectedEmptyText) Right . preview nonEmpty

-- | Strict parser for @Maybe NonEmptyText@ , fails on @Just ""@ values.
-- Also see 'lenientMaybeNonEmptyText'.
maybeNonEmptyText :: forall (ctx :: Maybe Symbol) . Step ctx (Maybe Text) (Maybe NonEmptyText)
maybeNonEmptyText = around nonEmptyText

-- | A lenient version of 'maybeNonEmptyText', which allows
-- @Just ""@ being parsed as @Nothing :: Maybe NonEmptyText@, compare:
--
-- >>> toEither $ parse (Just "") (maybeNonEmptyText)
-- Left [OtherParsingError UnexpectedEmptyText]
--
-- >>> toEither $ parse (Just "") (lenientMaybeNonEmptyText)
-- Right Nothing
--
-- Otherwise these two behave identically.
lenientMaybeNonEmptyText :: forall (ctx :: Maybe Symbol) . Step ctx (Maybe Text) (Maybe NonEmptyText)
lenientMaybeNonEmptyText = Step . Kleisli $
  maybe (Right Nothing) Right . fmap (preview nonEmpty)

-- | A 'Step' that ensures its numerical input is not a negative value.
--
-- Formerly 'notNegative'.
nonNegative :: forall (a :: Type) (ctx :: Maybe Symbol) .
  (Num a, Eq a, Typeable a) => Step ctx a a
nonNegative = Step . Kleisli $ \input -> case signum input of
  (-1) -> Left . UnexpectedNegative $ tyName @a
  _    -> Right input

-- | A 'Step' to check whether all characters are digits.
onlyDigits :: Step ctx NonEmptyText NonEmptyText
onlyDigits = liftEither go
  where
    go :: NonEmptyText -> Either ParsingErrorType NonEmptyText
    go t = if T.all isDigit (toText t)
      then Right t
      else Left $ Other "Only digits allowed here"

-- | Like 'nonEmptyText' but takes /minimal length/ and /maximal length/
-- as parameters. Enshrines the 'Text' is not empty in returning type
-- depending on the first type argument.
--
-- >>> toEither $ parse ("foo" :: Text) (range @0 @3)
-- Right "foo"
-- it :: Either (NonEmptyVector ParsingError) Text
--
-- >>> toEither $ parse ("foo" :: Text) (range @1 @3)
-- Right "foo"
-- it :: Either (NonEmptyVector ParsingError) NonEmptyText
range :: forall (minL :: TL.Nat) (maxL :: TL.Nat) (ctx :: Maybe Symbol)
  . (SNatI (FromGHC minL), KnownNat minL, KnownNat maxL)
  => Step ctx Text (TextByLength (FromGHC minL))
range = Step $ Kleisli $ case snat @(FromGHC minL) of
    SZ -> go
    SS -> \text ->
            let res = go text
            in mapRight (fromJust . preview nonEmpty) res
  where
    go :: Text -> Either ParsingErrorType Text
    go t = let
             tLength = toInteger $ T.length t
             maxLt = natVal (Proxy @maxL)
             minLt = natVal (Proxy @minL)
           in
             if tLength <= maxLt && tLength >= minLt
               then Right t
               else Left $ Other $ pack
                $ "Length should be >= " <> show minLt <> " and <= " <> show maxLt

-- | Calculates the text type going by its minimal length
type family TextByLength (minL :: Nat) where
  TextByLength  'Z    = Text
  TextByLength ('S _) = NonEmptyText

-- | A 'Step' which converts an 'a' into a 'Secret a'
-- The rule of thumb is to use `Secret` in types when it's possible.
-- When this cannot be done, e.g. for @beam@'s type definitions one can
-- use 'secret' for sensitive data fields to secure them.
--
-- >>> give SafelyHideSecrets $ show $ toEither $ parse ("password" :: Text) secret
-- "Right ***secret***"
secret :: Step ctx a (Secret a)
secret = Step . Kleisli $ Right . makeSecret

-- | A 'Step' which converts a 'LocalTime' into a 'UTCTime', on the assumption
-- that the input is zoned to UTC.
--
-- >>> date
-- 2012-09-21 00:00:00
-- it :: LocalTime
-- >>> toEither $ parse (date) toUTC
-- Right 2012-09-21 00:00:00 UTC
-- it :: Either (NonEmptyVector ParsingError) UTCTime
toUTC :: Step ctx LocalTime UTCTime
toUTC = Step . Kleisli $ Right . localTimeToUTC utc

{-------------------------------------------------------------------------------
  Helpers
-------------------------------------------------------------------------------}

intoNEVector :: NESeq a -> NonEmptyVector a
intoNEVector (h :<|| t) = unfoldr1 go h t
  where
    go :: Seq a -> Maybe (a, Seq a)
    go = \case
      Empty     -> Nothing
      x :<| acc -> Just (x, acc)

outOfNEVector :: NonEmptyVector a -> NESeq a
outOfNEVector = uncurry (:<||) . fmap (Seq.fromList . V.toList) . uncons

tyName :: forall (a :: Type) . (Typeable a) => Text
tyName = pack . tyConName . typeRepTyCon $ typeRep @a
