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
encodeJSON :: ToJSON a => a -> Text
encodeJSON = TL.toStrict . encodeToLazyText

-- | Parse JSON Text into a value
decodeJSON :: FromJSON a => Text -> Maybe a
decodeJSON = decode . BSL.fromStrict . TE.encodeUtf8

-- | Encode to Text
encodeT :: ToJSON a => a -> Text
encodeT = TE.decodeUtf8 . BSL.toStrict . encode

-- | Decode from Text
eitherDecodeT :: FromJSON a => Text -> Either String a
eitherDecodeT = eitherDecode . BSL.fromStrict . TE.encodeUtf8
