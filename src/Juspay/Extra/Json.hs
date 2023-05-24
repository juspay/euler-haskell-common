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
