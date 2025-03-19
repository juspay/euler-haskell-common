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

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-error=unused-top-binds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DefaultSignatures #-}

module Juspay.Extra.AES
  ( aesEncryptText
  , aesDecryptText
  , runAES
  )
where

import Crypto.Cipher.AES (AES256)
import Crypto.Cipher.Types (BlockCipher(..), Cipher(..), IV, makeIV)
import Crypto.Error (CryptoFailable(..))
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString as BS
import Data.Text

initializeAES :: BS.ByteString -> CryptoFailable AES256
initializeAES key = cipherInit key

createIV :: BS.ByteString -> Maybe (IV AES256)
createIV = makeIV

-- running AES in CTR mode
runAES :: BS.ByteString -> BS.ByteString -> BS.ByteString -> Either Text BS.ByteString
runAES key iv textBytes =
    case (initializeAES key, createIV iv) of
        (CryptoPassed cipher, Just iv') -> Right $ ctrCombine cipher iv' textBytes
        _ -> Left "Failed due to invalid key or IV."

aesEncryptText :: Text -> Text -> Text -> Either Text Text
aesEncryptText key iv plaintext = 
    let plaintextBS = TE.encodeUtf8 plaintext
        keyBS = TE.encodeUtf8 key
        ivBS = TE.encodeUtf8 iv
        encrypted = runAES keyBS ivBS plaintextBS
    in (TE.decodeUtf8 . B64.encode) <$> encrypted

aesDecryptText :: Text -> Text -> Text -> Either Text Text
aesDecryptText key iv ciphertext = do
    let ciphertextBS = TE.encodeUtf8 ciphertext
        keyBS = TE.encodeUtf8 key
        ivBS = TE.encodeUtf8 iv
    case B64.decode ciphertextBS of
        Right decodedBytes -> 
          TE.decodeUtf8 <$> runAES keyBS ivBS decodedBytes
        Left _ -> Left "Decryption failed due to invalid ciphertext format."