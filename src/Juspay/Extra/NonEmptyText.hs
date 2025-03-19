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

-- | A 'Prism' for converting between 'Text' and a newtype 'a' coercible to 'NonEmptyText'.
--
-- This 'Prism' is designed for use with newtypes that are coercible to 'NonEmptyText'.
-- It allows you to view and modify 'Text' values as values of the newtype 'a', and vice versa.
--
-- A prism is an optic that focuses on part of a larger structure. In the case of 'newtypeNetPrism',
-- it focuses on the presence or absence of characters in the 'Text', converting it to the newtype 'a'
-- if it is non-empty, and back to 'Text' otherwise. The 'newtypeNetPrism' prism is a partial isomorphism,
-- meaning it may fail in the direction from 'Text' to 'a' if the input 'Text' is empty.
--
-- Example:
-- >>> preview newtypeNetPrism "Hello" :: Maybe MyNewtype
-- >>> -- Just (MyNewtype (NET "Hello"))
--
-- >>> review newtypeNetPrism (MyNewtype (NET "World"))
-- >>> -- "World"
newtypeNetPrism :: forall a . (Coercible a NonEmptyText) => Prism' Text a
newtypeNetPrism = prism' out into
  where
    out = review nonEmpty . coerce @a @NonEmptyText
    into t = coerce @NonEmptyText @a <$> preview nonEmpty t
