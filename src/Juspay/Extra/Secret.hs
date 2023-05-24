-- TODO Given SecretContext vs. ExtractSecrets
-- TODO sequence vs. Traversable
-- TODO general approach -- cover as much as possible with Secret
{-#OPTIONS_GHC -fclear-plugins            #-}
{-#OPTIONS_GHC -Wno-unused-top-binds      #-}
{-#OPTIONS_GHC -Wno-redundant-constraints #-}
{-#LANGUAGE ConstraintKinds               #-}
{-#LANGUAGE DerivingStrategies            #-}
{-#LANGUAGE DerivingVia                   #-}
{-#LANGUAGE IncoherentInstances           #-}
{-#LANGUAGE OverloadedStrings             #-}
{-#LANGUAGE QuantifiedConstraints         #-}
{-#LANGUAGE RankNTypes                    #-}
{-#LANGUAGE RoleAnnotations               #-}
{-#LANGUAGE ScopedTypeVariables           #-}
{-#LANGUAGE TypeOperators                 #-}

-- | Protecting sensitive data from being accidentally leaked by
-- packing them into a 'Secret' newtype wrapper.
module Juspay.Extra.Secret
  (
    -- * Introduction
    -- $secret
    --

    -- * Core part
    SecretContext(..)
  , Secret -- do not export Secret's data Constructor for security reasons

    -- * Construction and extraction
  , makeSecret
  , ExtractSecrets
  , extractSecret
  , elimExtractSecrets
  , toBuilder
  , unsafeExtractSecret

    -- * HKD-style secreting
    -- $hkd
  , Secreted
  , unSecret

    -- * sequence-like transformations
  , sequenceSecretMaybe
  , sequenceMaybeSecret
  , sequenceSecretEither
  , sequenceEitherSecret

    -- * Servant stuff
    -- $servant
  , ProtectedJSON
  , DiscloseSecret (..)

    -- * Re-exports
    -- | Some of "Data.Reflection" are re-exported for users' convenience.
  , Given
  , give
  ) where

import Data.Aeson hiding (object)
import Data.ByteString.Builder (Builder, string8)
import Data.Coerce (coerce)
import Data.Functor.Contravariant (contramap)
import Data.Functor.Identity (Identity (Identity))
import Data.Kind (Type)
import Data.Reflection (Given, give, given)
import Data.Store (Store (..))
import Data.String (IsString)
import Data.Void (Void)
import GHC.Generics
import Network.HTTP.Media ((//))
import Prelude hiding (print)
import Servant.API (Accept(..), MimeRender(..))
import Test.QuickCheck (Arbitrary, Arbitrary1, arbitrary, arbitrary1, liftArbitrary)
import Web.FormUrlEncoded
import Web.HttpApiData
import qualified Data.Aeson as A
import qualified Data.Aeson.Encoding as AE
import qualified Data.HashMap.Strict as HM


-- $secret
--
-- This module introduces 'Secret' newtype wrapper. The idea is to put __all sensitive__
-- fields in a 'Secret` wrapper, so instead just @Text \/ Int \/ etc@ fields we are going
-- to work with @Secret Text@ and so on.
--
-- > data X = X {name :: Secret String, b :: Y}
-- >   deriving stock (Generic)
-- >   deriving anyclass (FromJSON)
-- >
-- >   deriving instance Given SecretContext => ToJSON X
-- >   deriving instance Given SecretContext => Show X
-- >   deriving instance Given SecretContext => Store X
-- >
-- > data Y = Y {c :: Int, money :: Secret Int}
-- >   deriving stock (Generic)
-- >   deriving anyclass (FromJSON, FromForm)
-- >
-- >   deriving instance Given SecretContext => ToJSON Y
-- >   deriving instance Given SecretContext => Show Y
-- >   deriving instance Given SecretContext => Store Y
-- >
--
-- The 'Secret' wrapper works as follows:
--
-- * you can put a value into a Secret using 'makeSecret' function
--
-- * in order to extract a value you have to provide @Given SecretContext@ instance
--
-- Let's start with creating:
--
-- > exampleX :: X
-- > exampleX = X { name = makeSecret "a", b = exampleY }
-- >
-- > exampleY :: Y
-- > exampleY = Y {c = 10, money = makeSecret 20}
--
-- Potentially dangerous instances including 'Show', 'ToJSON' and 'Store' also
-- require @Given SecretContext@. In order not to pass the context explicitly
-- this module leverages "Data.Reflection" machinery to pass around the context.
-- You don't need to understand how 'give'\/'given' is implemented but you
-- have to understand this:
--
-- >>> given (42 :: Int) $ given @Int
-- 42
--
-- Let's take a look at more examples:
--
-- >>> print $ give SafelyHideSecrets (A.encode exampleX)
-- "{\"name\":\"***secret***\",\"b\":{\"money\":\"***secret***\",\"c\":10}}"
-- >>> print $ give RiskyShowSecrets (A.encode exampleX)
-- "{\"name\":\"a\",\"b\":{\"money\":20,\"c\":10}}"
-- >>> print $ give SafelyHideSecrets (show exampleX :: String)
-- "X {name = ***secret***, b = Y {c = 10, money = ***secret***}}"
-- >>> print $ give RiskyShowSecrets (show exampleX :: String)
-- "X {name = \"a\", b = Y {c = 10, money = 20}}"
--
-- TODO: Is it needed?
--
-- 'FromJSON', 'FromForm', 'FromHttpApiData' and 'Store's parsing fails if
-- encounters /secreted data/:
--
-- >>> let secretJson = A.decode @X $ give SafelyHideSecrets $ A.encode exampleX
-- "*** Exception: FromJSON (Secret a): trying to decode secret data
--
-- For changing 'Secret' data just use 'Secret' as a monad without extraction:
--
-- > f :: Secret Int -> Secret String
-- > f secret = do
-- >  x <- secret
-- >  pure $ show $ x + 1

{-------------------------------------------------------------------------------
  Core part
-------------------------------------------------------------------------------}

-- | Secret context to annotate risky/safely usage of 'Secret' type
data SecretContext = RiskyShowSecrets | SafelyHideSecrets

-- | 'Secret' data type is used to hide any sensitive data stored in fields
-- in API and domain types.
--
-- We refused use the 'Traversable' instance intentionally since it opens
-- backdoors like @'traverse print (makeSecret password)'@. The same is the case
-- with 'Foldable' leading to extraction data, e.g.:
--
-- > sneaky :: Show a => Secret a -> String
-- > sneaky = foldMap show
newtype Secret (a :: Type) = Secret a
  deriving (Eq, Ord) via a
  deriving (Functor, Applicative, Monad) via Identity

instance Arbitrary a => Arbitrary (Secret a) where
  arbitrary = arbitrary1

instance Arbitrary1 Secret where
  liftArbitrary = fmap Secret

-- | Turns secreted values into __"***secret***"__ when in 'SafelyHideSecrets' context.
instance (Show a, Given SecretContext) => Show (Secret a) where
  show (Secret value) = case given of
    SafelyHideSecrets -> "***secret***"
    RiskyShowSecrets  -> show value

-- | Turns secreted values into __"***secret***"__ when in 'SafelyHideSecrets' context.
instance (ToJSON a, Given SecretContext) => ToJSON (Secret a) where
  toJSON (Secret value) = case given of
    SafelyHideSecrets -> A.String "***secret***"
    RiskyShowSecrets  -> toJSON value

  toEncoding (Secret value) = case given of
    SafelyHideSecrets -> AE.string "***secret***"
    RiskyShowSecrets  -> toEncoding value

-- | Fails when encountering __"***secret***"__ strings.
instance FromJSON a => FromJSON (Secret a) where
  parseJSON (String "***secret***") = Prelude.error "FromJSON (Secret a): trying to decode secret data"
  parseJSON value = makeSecret <$> parseJSON value

-- | Fails when encountering __"***secret***"__ strings.
instance FromForm a => FromForm (Secret a) where
  fromForm form = if HM.null $ HM.filter (elem "***secret***") $ unForm form
    then makeSecret <$> fromForm form
    else Left "FromForm (Secret a): trying to decode secret data"

-- | Fails when encountering __"***secret***"__ strings.
instance FromHttpApiData a => FromHttpApiData (Secret a) where
  parseUrlPiece "***secret***" = Left "FromHttpApiData (Secret a): trying to decode secret data"
  parseUrlPiece val = makeSecret <$> parseUrlPiece val

-- | SecretContext here used just to annotate unwrapping of 'Secret' values with 'Store' operations.
instance (Store a, Given SecretContext) => Store (Secret a) where
    size = case given of
      SafelyHideSecrets -> Prelude.error "Store.size (Secret a): trying to size secret data"
      RiskyShowSecrets  -> contramap (\(Secret x) -> x) size
    poke (Secret x) = case given of
      SafelyHideSecrets -> Prelude.error "Store.poke (Secret a): trying to poke secret data"
      RiskyShowSecrets  -> poke x
    peek = Secret <$> peek

{-------------------------------------------------------------------------------
  Building and extracting secrets
-------------------------------------------------------------------------------}

-- TODO add a show-case example

-- | Just makes a secret. When parsing values you might prefer to use a specialized
-- 'secret' step from "Juspay.Extra.Parsing"
makeSecret :: a -> Secret a
makeSecret = Secret

-- | Internal type class to avoid the use of `unsafeCoerce`.
class ExtractSecrets' dummy

-- By default type-classes' params get, so to say, the @nominal@ role, so for being
-- able to coerce @Wrap () a -> Wrap (UnDummy) a@ we need to ask compiler not to
-- strengthen 'Wrap''s @dummy@ param to nominal due to its usage with 'ExtractSecrets''
-- constraint.
type role ExtractSecrets' representational

-- | A constraint to track functions which requires access to sensitive data.
type ExtractSecrets = ExtractSecrets' ()

-- | A function to extract a value in 'ExtractSecrets' context.
extractSecret :: ExtractSecrets => Secret a -> a
extractSecret (Secret s) = s

-- | The last resort to extract sensitive data. Avoid to use it as as long as possible!
-- __It is NOT for show or logs!__
--
-- Please, annotate all uses of `unsafeExtractSecre` with a comment that says on why this
-- was done.
--
unsafeExtractSecret :: Secret a -> a
unsafeExtractSecret s = elimExtractSecrets (extractSecret s)

-- | TODO do we need it? If yes, why not to have @extract :: Given SecretContext => Secret a -> Maybe a@?
toBuilder :: Given SecretContext => Secret a -> (a -> Builder) -> Builder
toBuilder (Secret b) f = case given of
  SafelyHideSecrets -> string8 "***secret***"
  RiskyShowSecrets  -> f b

-- | Newtypes and instances to eliminate 'ExtractSecrets'' context
newtype Wrap dummy a = Wrap { unWrap :: ExtractSecrets' dummy => a }
newtype UnDummy = UnDummy ()
instance ExtractSecrets' UnDummy

-- | Eliminates @ExtractSecrets@ constraint.
--
-- >>> elimExtractSecrets $ extractSecret  $ Secret ("foo" :: String)
-- "foo"
elimExtractSecrets :: forall a. (ExtractSecrets => a) -> a
elimExtractSecrets v =
    unWrap (coerceWrap (Wrap v))
  where
    coerceWrap :: Wrap () a -> Wrap (UnDummy) a
    coerceWrap = coerce

{-------------------------------------------------------------------------------
  HKD-style secret elimination
-------------------------------------------------------------------------------}

-- $hkd
--

-- | Type family to write HKD-datatypes
type family Secreted  f a where
  Secreted Identity a = a
  Secreted Secret a = Secret a
  Secreted _ _ = Void

class UnSec i o where
  gUnSec :: i p -> o p

instance ExtractSecrets => UnSec (K1 a (Secret k)) (K1 a k) where
  -- gUnSec :: K1 a (Secret k) -> (K1 a k)
  gUnSec (K1 k) = K1 $ extractSecret k
  {-# INLINE gUnSec #-}

instance ExtractSecrets => UnSec (K1 a k) (K1 a k) where
  -- gUnSec :: K1 a k -> (K1 a k)
  gUnSec = id
  {-# INLINE gUnSec #-}

instance (UnSec i o, UnSec i' o')
    => UnSec (i :*: i') (o :*: o') where
  gUnSec (l :*: r) = gUnSec l :*: gUnSec r
  {-# INLINE gUnSec #-}

instance (UnSec i o, UnSec i' o')
    => UnSec (i :+: i') (o :+: o') where
  gUnSec (L1 l) = L1 $ gUnSec l
  gUnSec (R1 r) = R1 $ gUnSec r
  {-# INLINE gUnSec #-}

instance UnSec i o
    => UnSec (M1 _a _b i) (M1 _a' _b' o) where
  gUnSec (M1 x) = M1 $ gUnSec x
  {-# INLINE gUnSec #-}

instance UnSec V1 V1 where
  gUnSec = undefined
  {-# INLINE gUnSec #-}

instance UnSec U1 U1 where
  gUnSec U1 = U1
  {-# INLINE gUnSec #-}

unSecret
    :: forall f .
       ( Generic (f Secret)
       , Generic (f Identity)
       , UnSec (Rep (f Secret))
               (Rep (f Identity))
       )
    => f Secret
    -> f Identity
unSecret = to . gUnSec . from

{-------------------------------------------------------------------------------
  sequence-like transformations
-------------------------------------------------------------------------------}

sequenceSecretMaybe :: Secret (Maybe a) -> Maybe (Secret a)
sequenceSecretMaybe (Secret (Just a)) = Just $ makeSecret a
sequenceSecretMaybe _                 = Nothing

sequenceMaybeSecret :: Maybe (Secret a) -> Secret (Maybe a)
sequenceMaybeSecret (Just (Secret a)) = makeSecret $ Just a
sequenceMaybeSecret _                 = makeSecret Nothing

sequenceSecretEither :: (Given SecretContext, IsString e) => Secret (Either e a) -> Either e (Secret a)
sequenceSecretEither (Secret (Right a)) = Right $ makeSecret a
sequenceSecretEither (Secret (Left e)) = case given of
  SafelyHideSecrets -> Left "***secret***"
  RiskyShowSecrets  -> Left e

sequenceEitherSecret :: Either e (Secret a) -> Secret (Either e a)
sequenceEitherSecret (Right (Secret a)) = makeSecret $ Right a
sequenceEitherSecret (Left e)           = makeSecret $ Left e

{-------------------------------------------------------------------------------
  Some servant stuff
-------------------------------------------------------------------------------}

-- $servant
-- The 'Secret' is designed to be used with /servant/, one can use it in request
-- and response types. Nothing is needed to protect __requests__ since packing
-- into 'Secret' is a straightforward operation. For __responses__ just use
-- 'ProtectedJSON' instead of stock 'JSON' in your API-type and use /via deriving/
-- for @ToJSON@ instance using 'DiscloseSecret' newtype:
--
-- > type Api =
-- >   "path"
-- >   :> ReqBody '[JSON] Req        -- no changes here
-- >   :> Post '[ProtectedJSON] Foo  -- uses ProtectedJSON
-- >
-- > data Foo = Foo { bar :: Secret Text }
-- >
-- > deriving stock instance Eq InitTxnResp,
-- > deriving stock instance Generic InitTxnResp
-- > deriving anyclass instance Given SecretContext => ToJSON Foo
-- > deriving via (DiscloseSecret Foo) instance MimeRender ProtectedJSON Foo

-- | A promoted type constructor to use instead of the regular 'JSON' kind
data ProtectedJSON

instance Accept ProtectedJSON where
  contentType _ = "application" // "json"

-- | A newtype wrapper to use in /via deriving/
newtype DiscloseSecret a = DiscloseSecret a

-- | This instance uses __QuantifiedConstraints__ extension to express the fact that
-- @a@ is a @ToJSON@ only when a @Given SecretConstext@ is given. As such a context
-- 'DiscloseSecret' 'give's 'RiskyShowSecrets'.
instance (Given SecretContext => ToJSON a) => MimeRender ProtectedJSON (DiscloseSecret a) where
  mimeRender _ (DiscloseSecret v) = give RiskyShowSecrets $ encode v


-- TODO move to tests?

{-

-- import           Data.Text (Text, pack)
-- import           Euler.Types.Utils.JSONUtils (eitherDecodeT)
-- import Euler.Types.Utils.JSONUtils (encodeT)
-- import           GHC.Generics
-- import           Data.Reflection (give) -- leave here to test example
-- import qualified Data.Store as Binary -- leave here to test example

main :: IO ()
main = do
  let secretJson = A.decode @X $ give SafelyHideSecrets $ A.encode exampleX
  -- print $ give RiskyShowSecrets $ show secretJson
  print $ give SafelyHideSecrets (A.encode exampleX)
  print $ give RiskyShowSecrets (A.encode exampleX)
  print $ give SafelyHideSecrets (show exampleX :: String)
  print $ give RiskyShowSecrets (show exampleX :: String)
  P.putStrLn $ give RiskyShowSecrets $ show (urlDecodeAsForm @Y "money=***secret***&c=10")
  P.putStrLn $ give RiskyShowSecrets $ show (urlDecodeAsForm @Y "money=20&c=10")
  P.putStrLn $ give SafelyHideSecrets $ show (urlDecodeAsForm @Y "money=20&c=10")
  P.putStrLn "Store examples: "
  let binRisk = give RiskyShowSecrets $ Binary.encode exampleX
  P.putStrLn $ give RiskyShowSecrets $ show (Binary.decode @X binRisk)
  P.putStrLn $ give SafelyHideSecrets $ show (Binary.decode @X binRisk)
  let binSafe = give SafelyHideSecrets $ Binary.encode exampleX
  P.putStrLn $ give RiskyShowSecrets $ show (Binary.decode @X binSafe)
  P.putStrLn $ give SafelyHideSecrets $ show (Binary.decode @X binSafe)

-- "*** Exception: FromJSON (Secret a): trying to decode secret data
-- "{\"name\":\"***secret***\",\"b\":{\"money\":\"***secret***\",\"c\":10}}"
-- "{\"name\":\"a\",\"b\":{\"money\":20,\"c\":10}}"
-- "X {name = ***secret***, b = Y {c = 10, money = ***secret***}}"
-- "X {name = \"a\", b = Y {c = 10, money = 20}}"
-- Left "FromHttpApiData (Secret a): trying to decode secret data"
-- Right (Y {c = 10, money = 20})
-- Right (Y {c = 10, money = ***secret***})
-- Store examples:
-- Right (X {name = "a", b = Y {c = 10, money = 20}})
-- Right (X {name = ***secret***, b = Y {c = 10, money = ***secret***}})
-- Right (X {name = "a", b = Y {c = 10, money = 20}})
-- *** Exception: Store.size (Secret a): trying to size secret data
-- *** Exception: Store.size (Secret a): trying to size secret data
-}
