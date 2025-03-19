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

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Juspay.Extra.Env where

import Control.Monad.Catch (MonadThrow)
import qualified Data.Aeson as A
import Data.Maybe
import Data.Text hiding (toUpper,toTitle)
import Data.Char (toUpper)
import System.IO.Unsafe (unsafePerformIO)
import Text.Read (readMaybe)
import Unsafe.Coerce (unsafeCoerce)
import Type.Reflection
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified System.Environment as S

data JuspayEnv m a b = JuspayEnv
    { key :: Text
    , actionLeft :: EnvAction m b
    , decryptFunc :: a -> m b
    , logWhenThrowException :: Maybe (m ())
    }

data EnvAction m b = DEFAULT_VALUE b | ERROR_EXCEPTION (m b)

mkDefaultEnvAction ::  a -> EnvAction m a
mkDefaultEnvAction = DEFAULT_VALUE

mkErrorEnvAction :: (m b) -> EnvAction m b
mkErrorEnvAction = ERROR_EXCEPTION

lookupEnv :: (Read a,Typeable b,A.FromJSON a,Show a) => (JuspayEnv IO a b) -> b
lookupEnv = unsafePerformIO . lookupEnvM

lookupEnvM :: forall m a b. (MonadThrow m, Read a, Typeable b,A.FromJSON a,Show a) => JuspayEnv m a b -> m b
lookupEnvM juspayEnv = case actionLeft juspayEnv of
    DEFAULT_VALUE defaultValue -> handleLookup (pure defaultValue)
    ERROR_EXCEPTION errAction -> handleLookup (errorHandling errAction)
  where
        handleLookup defaultAction = do
            envValue <- pure $ unsafePerformIO $ S.lookupEnv (Data.Text.unpack $ key juspayEnv)
            case envValue of
                Nothing -> do
                    defaultAction
                Just value -> case parseValue value of
                                Left (_ :: String) -> defaultAction
                                Right parsedValue -> decryptFunc juspayEnv (parsedValue)

        errorHandling errH = do
            fromMaybe (pure ()) (logWhenThrowException juspayEnv)
            errH

        parseValue value
            | Just HRefl <- eqTypeRep (typeRep @b) (typeRep @String) = Right $ unsafeCoerce value
            | Just HRefl <- eqTypeRep (typeRep @b) (typeRep @Text) = Right $ unsafeCoerce $ Data.Text.pack value
            | Just HRefl <- eqTypeRep (typeRep @b) (typeRep @(Maybe String)) = Right $ unsafeCoerce $ Just value
            | Just HRefl <- eqTypeRep (typeRep @b) (typeRep @(Maybe Text)) = Right $ unsafeCoerce $ Just $ Data.Text.pack value
            | Just HRefl <- eqTypeRep (typeRep @b) (typeRep @(Maybe [String])) =
                                    case A.decode (BL.pack value) of
                                        Nothing -> Right $ unsafeCoerce (Nothing :: Maybe [String])
                                        Just result -> Right $ unsafeCoerce (Just result :: Maybe [String])
            | Just HRefl <- eqTypeRep (typeRep @b) (typeRep @(Maybe [Text])) =
                                    case A.decode (BL.pack value) of
                                        Nothing -> Right $ unsafeCoerce (Nothing :: Maybe [Text])
                                        Just result -> Right $ unsafeCoerce (Just result :: Maybe [Text])
            | otherwise = case readMaybe (transformForBool value) of
                Just x -> Right x
                Nothing -> do
                    case tyConName $ fst $ splitApps (typeRep @b) of
                        "Maybe" -> maybe (Left "Failed to parse value") (Right) $ readMaybe ("Just" <> " " <> value)
                        _ -> A.eitherDecode $ A.encode value

        transformForBool x =
            let x' =
                    case x of
                        "true"  -> toTitle x
                        "false" -> toTitle x
                        _ -> x
            in case eqTypeRep (typeRep @b) (typeRep @Bool) of
                    Just HRefl -> x'
                    _     -> x

        toTitle :: String -> String
        toTitle ""     = ""
        toTitle (x:xs) = toUpper x : xs