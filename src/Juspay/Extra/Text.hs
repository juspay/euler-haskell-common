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

-- | 'Text' - related extra functions
module Juspay.Extra.Text
 ( -- * Extra functions
   showT
 , padRightT
 , readMaybeT
 , isBlank
 , isNotBlank
 , notBlank
 , isBlankM
 , isNotBlankM
 , isBlankMS
 , isNotBlankMS
 , isFalseMaybe
 , isTrueMaybe
 , isNotTrueMaybe
 , toTitle
 , emptyTextAsNothing
   -- * Euler-compatible aliases
 , isBlankMaybe
 , isFullMaybe
 , isFullMaybeSecret
   -- * Common aliases
 , toLazy
 -- * Make request body from query parametors
 , formUrlEncode
 ) where

import qualified Data.ByteString as B
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as Builder
import Data.ByteString.Lazy (fromStrict)
import qualified Data.ByteString.Lazy as BL
import Data.Char (toLower, toUpper)
import qualified Data.Char as C
import Data.List (intersperse)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Buildable (Buildable)
import qualified Data.Text.Encoding as TE
import Data.Text.Format (right)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText)
import Data.Word (Word8)
import Juspay.Extra.Secret (Secret)
import qualified Juspay.Extra.Secret as Secret
import Text.Read (readMaybe)


-- | Shows any 'Show' as 'Text'.
showT :: Show a => a -> Text
showT = T.pack . show

-- | Adds spaves to right-hand size of a text.
padRightT :: Buildable a => Int -> a -> Text
padRightT size = toStrict . toLazyText . right size ' '

-- | Alias for 'fromStrict'.
toLazy :: B.ByteString -> BL.ByteString
toLazy = fromStrict

-- | 'readMaybe' analugue for 'Text'.
-- formerly @readMay@
readMaybeT :: Read a => Text -> Maybe a
readMaybeT = readMaybe . T.unpack

-- | Check whether a text is empty after striping leading and trailing spaces.
isBlank :: Text -> Bool
isBlank = T.null . T.strip

-- | Check whether text is not empty after striping trailing spaces
isNotBlank ::Text -> Bool
isNotBlank =  not . isBlank

-- | Check whether text is empty after striping trailing spaces
-- You may consider using "Juspay.Extra.Parsing" module instead of this function.
notBlank :: Text -> Maybe Text
notBlank t
  | isBlank t = Nothing
  | otherwise = Just t

-- | Checks whether a @Maybe Text@ is empty
isBlankM :: Maybe Text -> Bool
isBlankM Nothing    = False -- quite arguable is you ask me!
isBlankM (Just val) = isBlank val

isBlankMaybe :: Maybe Text -> Bool
isBlankMaybe = isBlankM

-- | Checks whether @Maybe Text@ is not empty
isNotBlankM :: Maybe Text -> Bool
isNotBlankM Nothing    = False
isNotBlankM (Just val) = isNotBlank val

isFullMaybe :: Maybe Text -> Bool
isFullMaybe = isNotBlankM

-- | Checks whether text in Maybe Secret is empty
isBlankMS :: Maybe (Secret Text) -> Bool
isBlankMS Nothing    = False -- quite arguable is you ask me!
isBlankMS (Just val) = (isBlank <$> val) == Secret.makeSecret False

-- | Checks whether text in Maybe Secret is not empty
isNotBlankMS :: Maybe (Secret Text) -> Bool
isNotBlankMS Nothing    = False
isNotBlankMS (Just val) = (isNotBlank <$> val) == Secret.makeSecret True

isFullMaybeSecret :: Maybe (Secret Text) -> Bool
isFullMaybeSecret = isNotBlankMS

-- | Checks whether 'Bool' inside 'Maybe' is @False@ value or not, i.e.:
-- @Just False@ -> @True@
-- @Just True@ -> @False@
-- @Nothing@ -> @False@
isFalseMaybe :: Maybe Bool -> Bool
isFalseMaybe (Just False) = True
isFalseMaybe _            = False

-- | Checks whether 'Bool' inside 'Maybe' is @True@ value or not, i.e.:
-- @Just True@ -> @True@
-- @Just False@ -> @False@
-- @Nothing@ -> @False@
isTrueMaybe :: Maybe Bool -> Bool
isTrueMaybe (Just True) = True
isTrueMaybe _           = False

-- | A reverse of 'isTrueMaybe' - checks whether a 'Bool' inside 'Maybe' is 'False' or not, i.e.:
-- @Just True@ -> @False@
-- @Just False@ -> @True@
-- @Nothing@ -> @True@
isNotTrueMaybe :: Maybe Bool -> Bool
isNotTrueMaybe = not . isTrueMaybe

-- | Normalize the \"capitalization\" of a 'String'.
toTitle :: String -> String
toTitle ""     = ""
toTitle (x:xs) = toUpper x : map toLower xs

-- A helper function which interprets an empty 'Text' inside 'Maybe' as 'Nothing'.
emptyTextAsNothing :: Maybe Text -> Maybe Text
emptyTextAsNothing Nothing   = Nothing
emptyTextAsNothing (Just "") = Nothing
emptyTextAsNothing x         = x

-- | Encode a list of key-value pairs as a URL-encoded 'ByteString' for use in a request body.
-- Make request body from query parametors
--
-- This function takes a list of key-value pairs and encodes them into a URL-encoded 'ByteString'
-- suitable for use as a request body in HTTP requests. The encoding follows the standard form
-- of key-value pairs separated by '&' and each pair consisting of key and value separated by '='.
--
-- The encoding of each key and value follows URL encoding rules, where reserved characters are percent-encoded.
--
-- Example:
-- >>> let params = [("name", "John Doe"), ("age", "30")]
-- >>> formUrlEncode params
-- >>> -- "name=John%20Doe&age=30"
formUrlEncode :: [(Text, Text)] -> BL.ByteString
formUrlEncode = Builder.toLazyByteString . mconcat . intersperse amp . map encodePair
  where
    equals = Builder.word8 (ord '=')
    amp = Builder.word8 (ord '&')
    percent = Builder.word8 (ord '%')
    plus = Builder.word8 (ord '+')

    -- Encode a key-value pair
    encodePair :: (Text, Text) -> Builder
    encodePair (key, value) = encode key <> equals <> encode value

    -- Encode a Text value
    encode :: Text -> Builder
    encode = escape . TE.encodeUtf8

    -- URL-encode a ByteString
    escape :: B.ByteString -> Builder
    escape = mconcat . map f . B.unpack
      where
        -- Encode each character individually
        f :: Word8 -> Builder
        f c
          | p c = Builder.word8 c
          | c == ord ' ' = plus
          | otherwise = percentEncode c

        -- Predicate for characters that do not need encoding
        p :: Word8 -> Bool
        p c =
             ord 'a' <= c && c <= ord 'z'
          || c == ord '_'
          || c == ord '*'
          || c == ord '-'
          || c == ord '.'
          || ord '0' <= c && c <= ord '9'
          || ord 'A' <= c && c <= ord 'Z'

    -- Convert a Char to Word8
    ord :: Char -> Word8
    ord = fromIntegral . C.ord

    -- Percent-encode a Word8
    percentEncode :: Word8 -> Builder
    percentEncode n = percent <> hex hi <> hex lo
      where
        (hi, lo) = n `divMod` 16

    -- Convert a hex value to Word8
    hex :: Word8 -> Builder
    hex n = Builder.word8 (offset + n)
      where
        offset
          | n < 10    = 48
          | otherwise = 55
