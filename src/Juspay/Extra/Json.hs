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

module Juspay.Extra.Json where

import           Data.Aeson           (FromJSON, ToJSON, decode, eitherDecode,
                                       encode)
import           Data.Aeson.Text      (encodeToLazyText)
import qualified Data.ByteString.Lazy as BSL
import           Data.Text            (Text)
import qualified Data.Text.Encoding   as TE
import qualified Data.Text.Lazy       as TL

-- | Encode a value to JSON Text
--
-- Note: the name `jsonEncode` is already taken by Aeson

-- | Encodes a value to JSON format as 'Text'.
--
-- This function takes a value of type 'a' that has an instance of the 'ToJSON' class
-- and converts it to its JSON representation as 'Text'. The 'encodeToLazyText'
-- function from the 'aeson' library is used for encoding, and the resulting
-- lazy 'Text' is then converted to strict 'Text'.
--
-- Example:
-- >>> let jsonString = encodeJSON ["apple", "banana", "orange"]
-- >>> -- jsonString :: Text
--
-- Note :
-- encodeJSON works with lazy text, whereas encodeT works with strict text.
-- encodeJSON directly uses encodeToLazyText for encoding, while encodeT uses encode and involves additional conversions from lazy bytestring to strict bytestring and then to text.
-- In terms of efficiency and performance, using lazy text (encodeJSON) may be more memory-efficient for large JSON data since it can be streamed and processed lazily. On the other hand, using strict text (encodeT) may be simpler and more straightforward for smaller data or when strict evaluation is preferred.
encodeJSON :: ToJSON a => a -> Text
encodeJSON = TL.toStrict . encodeToLazyText

-- | Parse JSON Text into a value
-- Decodes a JSON-encoded 'Text' into a value of type 'a'.
--
-- This function takes a JSON-encoded 'Text' and attempts to decode it into a
-- value of type 'a' using the 'decode' function from the 'aeson' library.
-- The input 'Text' is first converted to strict 'ByteString' to match the
-- expected input type of 'decode'.
--
-- The result is a 'Maybe' value where 'Just' contains the decoded value if
-- successful, and 'Nothing' if the decoding fails.
--
-- Example:
-- >>> let jsonText = "{\"fruit\":\"apple\",\"quantity\":3}"
-- >>> decodeJSON jsonText :: Maybe FruitInfo
decodeJSON :: FromJSON a => Text -> Maybe a
decodeJSON = decode . BSL.fromStrict . TE.encodeUtf8

-- | Encode to Text

-- | Encodes a value to JSON format as 'Text'.
--
-- This function takes a value of type 'a' that has an instance of the 'ToJSON' class
-- and converts it to its JSON representation as 'Text'. The 'encode' function from
-- the 'aeson' library is used for encoding, and the resulting lazy 'ByteString' is
-- first converted to strict 'ByteString' before being decoded to 'Text'.
--
-- Example:
-- >>> let jsonString = encodeT ["apple", "banana", "orange"]
-- >>> -- jsonString :: Text
-- 
-- Note :
-- encodeJSON works with lazy text, whereas encodeT works with strict text.
-- encodeJSON directly uses encodeToLazyText for encoding, while encodeT uses encode and involves additional conversions from lazy bytestring to strict bytestring and then to text.
-- In terms of efficiency and performance, using lazy text (encodeJSON) may be more memory-efficient for large JSON data since it can be streamed and processed lazily. On the other hand, using strict text (encodeT) may be simpler and more straightforward for smaller data or when strict evaluation is preferred.
encodeT :: ToJSON a => a -> Text
encodeT = TE.decodeUtf8 . BSL.toStrict . encode

-- | Decodes a JSON-encoded 'Text' into an 'Either' value with a potential decoding error.
--
-- This function takes a JSON-encoded 'Text' and attempts to decode it into a value of type 'a'
-- using the 'eitherDecode' function from the 'aeson' library. The input 'Text' is first
-- converted to strict 'ByteString' to match the expected input type of 'eitherDecode'.
--
-- The result is an 'Either' value where 'Right' contains the decoded value if successful,
-- and 'Left' contains a string error message if the decoding fails.
--
-- Example:
-- >>> let jsonText = "{\"fruit\":\"apple\",\"quantity\":3}"
-- >>> eitherDecodeT jsonText :: Either String FruitInfo
eitherDecodeT :: FromJSON a => Text -> Either String a
eitherDecodeT = eitherDecode . BSL.fromStrict . TE.encodeUtf8
