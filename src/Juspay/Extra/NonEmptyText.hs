{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Juspay.Extra.NonEmptyText
  ( NonEmptyText,
    fromText,
    nonEmpty,
    new,
    newtypeNetPrism,
    toText,
  )
where

import           Prelude
import           Control.DeepSeq (NFData)
import           Data.Aeson (FromJSON(..), ToJSON(..), withText)
import           Data.Aeson.Types (prependFailure)
import           Data.Coerce (Coercible, coerce)
import           Data.Functor (($>))
import qualified Data.Store as Binary
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.TreeDiff (ToExpr)
import           Optics.Core (preview, review)
import           Optics.Prism (Prism', prism')
import           Test.QuickCheck (Arbitrary (arbitrary, shrink), listOf)
import           GHC.Generics(Generic)

-- | An opaque wrapper for 'Text' which cannot be empty.
newtype NonEmptyText = NET {toText :: Text}
  deriving newtype (Eq, Ord, Show, Semigroup, ToJSON, Binary.Store)
  deriving stock Generic
  deriving anyclass (ToExpr, NFData)

instance FromJSON NonEmptyText where
  parseJSON = prependFailure "Parsing of the NonEmptyText value failed: " . withText "NonEmptyText" go
    where
      go t = case Text.uncons t of
        Nothing -> fail "Tried to convert an empty Text"
        Just _  -> pure . NET $ t

instance Arbitrary NonEmptyText where
  arbitrary = do
    h <- arbitrary @Char
    t <- listOf @Char arbitrary
    pure . NET . Text.pack $ h : t
  shrink =
    fmap NET . shrinkText . toText
    where
      shrinkText text = Text.pack <$> filter (not . null) (shrink (Text.unpack text))

-- | Create a `NonEmptyText` from `Text`
fromText :: Text -> Maybe NonEmptyText
fromText "" = Nothing
fromText t  = Just (NET t)

-- | Move between 'Text' and 'NonEmptyText' conveniently.
nonEmpty :: Prism' Text NonEmptyText
nonEmpty = prism' toText (\t -> Text.uncons t $> NET t)

-- | Create a 'NonEmptyText' from a \'head\' and \'tail\'.
new :: Char -> Text -> NonEmptyText
new c = NET . Text.cons c

-- | Create prism for newtypes over NonEmptyText
newtypeNetPrism :: forall a . (Coercible a NonEmptyText) => Prism' Text a
newtypeNetPrism = prism' out into
  where
    out = review nonEmpty . coerce @a @NonEmptyText
    into t = coerce @NonEmptyText @a <$> preview nonEmpty t
