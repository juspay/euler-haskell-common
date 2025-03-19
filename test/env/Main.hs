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

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-error=name-shadowing #-}

module Main where

import Control.Concurrent.Async (concurrently)
import Control.Exception (finally)
import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HM
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import qualified Data.Vector as V
import Debug.Trace (traceShowId)
import GHC.Generics (Generic)
import Juspay.Extra.Env
import qualified System.Environment as Env
import Test.Hspec
import Test.Hspec.Runner
import Universum.String.Conversion (encodeUtf8)
import qualified Crypto.Hash as Hash
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as BS8
import qualified Juspay.Extra.AES as EE

setupEnv :: [(String, String)] -> IO ()
setupEnv vars = do
  mapM_ (Env.unsetEnv . fst) vars
  mapM_ (uncurry Env.setEnv) vars

cleanupEnv :: [String] -> IO ()
cleanupEnv = mapM_ Env.unsetEnv

mockDecrypt :: a -> IO a
mockDecrypt = pure

mockErrorLogger :: IO ()
mockErrorLogger = pure ()

cleanupTestEnv :: IO ()
cleanupTestEnv = do
  let testVars =
        [ "TEST_STRING",
          "TEXT_VAR",
          "UNICODE_TEXT",
          "MAYBE_STRING",
          "MAYBE_TEXT",
          "BOOL_TRUE",
          "BOOL_FALSE",
          "BOOL_INVALID",
          "STRING_LIST",
          "TEXT_LIST",
          "INVALID_LIST",
          "BAD_INT",
          "INT_VAR",
          "NEG_INT",
          "COMPLEX",
          "COMBINED"
        ]
  mapM_ Env.unsetEnv testVars

main :: IO ()
main = do
  putStrLn "\nStarting Juspay Environment Framework Tests...\n"

  setupInitialEnv

  finally
    (runTestSuite)
    cleanupTestEnv
  where
    setupInitialEnv :: IO ()
    setupInitialEnv = do
      cleanupTestEnv
      Env.setEnv "TEST_MODE" "true"

runTestSuite :: IO ()
runTestSuite = do
  let hspecConfig = Test.Hspec.Runner.defaultConfig
  hspecWith hspecConfig spec

data ServiceConfig = ServiceConfig
  { name :: Text,
    port :: Int,
    timeout :: Int,
    retries :: Int,
    enabled :: Bool,
    endpoints :: Maybe [Text],
    secrets :: Maybe Text,
    metadata :: Maybe A.Value
  }
  deriving (Show, Eq, Generic)

instance A.FromJSON ServiceConfig

instance A.ToJSON ServiceConfig

data DatabaseConfig = DatabaseConfig
  { dbHost :: Text,
    dbPort :: Int,
    dbName :: Text,
    dbUser :: Text,
    dbPassword :: Text,
    dbPoolSize :: Int,
    dbTimeout :: Int,
    dbSSL :: Bool
  }
  deriving (Show, Eq, Generic)

instance A.FromJSON DatabaseConfig

instance A.ToJSON DatabaseConfig



spec :: Spec
spec = do
  -- 1. Basic String Tests
  describe "String Environment Variables" $ do
    it "retrieves existing string variable" $ do
      setupEnv [("TEST_STRING", "hello world")]
      let env = JuspayEnv "TEST_STRING" (mkDefaultEnvAction ("" :: String)) mockDecrypt Nothing
      result <- lookupEnvM env
      result `shouldBe` ("hello world" :: String)
      cleanupEnv ["TEST_STRING"]

    it "returns default when variable missing" $ do
      let env = JuspayEnv "MISSING_STRING" (mkDefaultEnvAction ("default" :: String)) mockDecrypt Nothing
      result <- lookupEnvM env
      result `shouldBe` ("default" :: String)

  describe "String Environment Variables" $ do
    it "retrieves existing string variable" $ do
      cleanupEnv ["TXN_SIMULATOR_PAGE_BASE64"]
      let env = JuspayEnv "TXN_SIMULATOR_PAGE_BASE64" (mkDefaultEnvAction (Just "" :: Maybe String)) mockDecrypt Nothing
      result <- (lookupEnvM env :: IO (Maybe String))
      result `shouldBe` (Just "" :: Maybe String)
      cleanupEnv ["TXN_SIMULATOR_PAGE_BASE64"]

  -- 2. Text Type Tests
  describe "Text Environment Variables" $ do
    it "converts string to Text" $ do
      setupEnv [("TEXT_VAR", "hello")]
      let env = JuspayEnv "TEXT_VAR" (mkDefaultEnvAction ("" :: Text)) mockDecrypt Nothing
      result <- lookupEnvM env :: IO Text
      result `shouldBe` "hello"
      cleanupEnv ["TEXT_VAR"]

    it "handles Unicode in Text" $ do
      setupEnv [("UNICODE_TEXT", "Hello 世界")]
      let env = JuspayEnv "UNICODE_TEXT" (mkDefaultEnvAction ("" :: Text)) mockDecrypt Nothing
      result <- lookupEnvM env :: IO Text
      result `shouldBe` "Hello 世界"
      cleanupEnv ["UNICODE_TEXT"]

  -- 3. Maybe Type Tests
  describe "Maybe Environment Variables" $ do
    it "handles Just String value" $ do
      setupEnv [("MAYBE_STRING", "value")]
      let env = JuspayEnv "MAYBE_STRING" (mkDefaultEnvAction Nothing) mockDecrypt Nothing
      result <- lookupEnvM env :: IO (Maybe String)
      result `shouldBe` Just "value"
      cleanupEnv ["MAYBE_STRING"]

    it "handles Just Text value" $ do
      setupEnv [("MAYBE_TEXT", "value")]
      let env = JuspayEnv "MAYBE_TEXT" (mkDefaultEnvAction Nothing) mockDecrypt Nothing
      result <- lookupEnvM env :: IO (Maybe Text)
      result `shouldBe` Just "value"
      cleanupEnv ["MAYBE_TEXT"]

    it "handles Nothing for missing variable" $ do
      let env = JuspayEnv "MAYBE_MISSING" (mkDefaultEnvAction Nothing) mockDecrypt Nothing
      result <- lookupEnvM env :: IO (Maybe String)
      result `shouldBe` Nothing

  -- 4. Boolean Tests
  describe "Boolean Environment Variables" $ do
    it "parses 'true' correctly" $ do
      setupEnv [("BOOL_TRUE", "true")]
      let env = JuspayEnv "BOOL_TRUE" (mkDefaultEnvAction False) mockDecrypt Nothing
      result <- lookupEnvM env :: IO Bool
      result `shouldBe` True
      cleanupEnv ["BOOL_TRUE"]

    it "parses 'false' correctly" $ do
      setupEnv [("BOOL_FALSE", "false")]
      let env = JuspayEnv "BOOL_FALSE" (mkDefaultEnvAction True) mockDecrypt Nothing
      result <- lookupEnvM env :: IO Bool
      result `shouldBe` False
      cleanupEnv ["BOOL_FALSE"]

    it "handles invalid boolean with default" $ do
      setupEnv [("BOOL_INVALID", "not-a-bool")]
      let env = JuspayEnv "BOOL_INVALID" (mkDefaultEnvAction False) mockDecrypt Nothing
      result <- lookupEnvM env :: IO Bool
      result `shouldBe` False
      cleanupEnv ["BOOL_INVALID"]

  -- 5. List Tests
  describe "List Environment Variables" $ do
    it "parses Maybe [String] from JSON" $ do
      setupEnv [("STRING_LIST", "[\"a\",\"b\",\"c\"]")]
      let env = JuspayEnv "STRING_LIST" (mkDefaultEnvAction Nothing) mockDecrypt Nothing
      result <- lookupEnvM env :: IO (Maybe [String])
      result `shouldBe` Just ["a", "b", "c"]
      cleanupEnv ["STRING_LIST"]

    it "parses Maybe [Text] from JSON" $ do
      setupEnv [("TEXT_LIST", "[\"x\",\"y\",\"z\"]")]
      let env = JuspayEnv "TEXT_LIST" (mkDefaultEnvAction Nothing) mockDecrypt Nothing
      result <- lookupEnvM env :: IO (Maybe [Text])
      result `shouldBe` Just ["x", "y", "z"]
      cleanupEnv ["TEXT_LIST"]

    it "handles invalid JSON for lists" $ do
      setupEnv [("INVALID_LIST", "not-json")]
      let env = JuspayEnv "INVALID_LIST" (mkDefaultEnvAction Nothing) mockDecrypt Nothing
      result <- lookupEnvM env :: IO (Maybe [String])
      result `shouldBe` Nothing
      cleanupEnv ["INVALID_LIST"]

  -- 6. Error Handling Tests
  describe "Error Handling" $ do
    it "uses error action when variable missing" $ do
      let env = JuspayEnv "REQUIRED" (mkErrorEnvAction $ pure "error-value") mockDecrypt (Just mockErrorLogger)
      result <- lookupEnvM env :: IO String
      result `shouldBe` "error-value"

    it "uses default for unparseable value" $ do
      setupEnv [("BAD_INT", "not-an-int")]
      let env = JuspayEnv "BAD_INT" (mkDefaultEnvAction (42 :: Int)) mockDecrypt Nothing
      result <- lookupEnvM env
      result `shouldBe` (42 :: Int)
      cleanupEnv ["BAD_INT"]

  -- 7. Numeric Type Tests
  describe "Numeric Environment Variables" $ do
    it "parses valid integer" $ do
      setupEnv [("INT_VAR", "42")]
      let env = JuspayEnv "INT_VAR" (mkDefaultEnvAction (0 :: Int)) mockDecrypt Nothing
      result <- lookupEnvM env
      result `shouldBe` (42 :: Int)
      cleanupEnv ["INT_VAR"]

    it "parses negative integer" $ do
      setupEnv [("NEG_INT", "-42")]
      let env = JuspayEnv "NEG_INT" (mkDefaultEnvAction (0 :: Int)) mockDecrypt Nothing
      result <- lookupEnvM env
      result `shouldBe` (-42 :: Int)
      cleanupEnv ["NEG_INT"]

  -- 8. Integration Tests
  describe "Integration Scenarios" $ do
    it "handles complex transformation chain" $ do
      setupEnv [("COMPLEX", "42")]
      let env = JuspayEnv "COMPLEX" (mkDefaultEnvAction (0 :: Int)) (\x -> pure (x * 2)) Nothing
      result <- lookupEnvM env
      result `shouldBe` (84 :: Int)
      cleanupEnv ["COMPLEX"]

    it "combines error logging and default values" $ do
      setupEnv [("COMBINED", "invalid")]
      let env = JuspayEnv "COMBINED" (mkDefaultEnvAction (0 :: Int)) mockDecrypt (Just mockErrorLogger)
      result <- lookupEnvM env
      result `shouldBe` (0 :: Int)
      cleanupEnv ["COMBINED"]
  -- 9. Complex JSON Configuration Tests
  describe "Complex JSON Configuration" $ do
    it "parses nested JSON configuration" $ do
      let jsonConfig :: String
          jsonConfig = "{\"name\":\"test-service\",\"port\":8080,\"timeout\":30,\"retries\":3,\"enabled\":true,\"endpoints\":[\"api.test.com\"],\"secrets\":\"encrypted:key\",\"metadata\":{\"version\":\"1.0\"}}"

      setupEnv [("SERVICE_CONFIG", jsonConfig)]
      let env :: JuspayEnv IO String ServiceConfig
          env =
            JuspayEnv
              { key = "SERVICE_CONFIG",
                actionLeft = mkDefaultEnvAction (error ("Missing config" :: String)),
                decryptFunc = (\(x :: String) -> (either (error) (pure) . A.eitherDecode . encodeUtf8 . traceShowId) x),
                logWhenThrowException = Nothing
              }
      result <- lookupEnvM env :: IO ServiceConfig
      port result `shouldBe` (8080 :: Int)
      enabled result `shouldBe` True
      cleanupEnv ["SERVICE_CONFIG"]

    it "handles complex database configuration" $ do
      let dbConfig :: String
          dbConfig = "{\"dbHost\":\"localhost\",\"dbPort\":5432,\"dbName\":\"testdb\",\"dbUser\":\"user\",\"dbPassword\":\"pass\",\"dbPoolSize\":10,\"dbTimeout\":30,\"dbSSL\":true}"

      setupEnv [("DB_CONFIG", dbConfig)]

      let env :: JuspayEnv IO String DatabaseConfig
          env =
            JuspayEnv
              { key = "DB_CONFIG",
                actionLeft = mkDefaultEnvAction (error "Missing DB config"),
                decryptFunc = (\(x :: String) -> (either (error) (pure) . A.eitherDecode . encodeUtf8 . traceShowId) x),
                logWhenThrowException = Nothing
              }

      result <- lookupEnvM env :: IO DatabaseConfig
      dbPort result `shouldBe` (5432 :: Int)
      dbPoolSize result `shouldBe` (10 :: Int)
      cleanupEnv ["DB_CONFIG"]

  describe "Nested Environment Variables" $ do
    it "handles nested environment references" $ do
      let envVars :: [(String, String)]
          envVars =
            [ ("BASE_URL", "http://api.example.com"),
              ("API_PATH", "${BASE_URL}/v1"),
              ("FULL_PATH", "${API_PATH}/users")
            ]

      setupEnv envVars

      let resolveVar :: String -> IO String
          resolveVar value = do
            if "${" `T.isInfixOf` T.pack value
              then
                pure $
                  T.unpack $
                    T.replace "${BASE_URL}" "http://api.example.com" $
                      T.replace "${API_PATH}" "http://api.example.com/v1" $
                        T.pack value
              else pure value

          env :: JuspayEnv IO String String
          env =
            JuspayEnv
              { key = "FULL_PATH",
                actionLeft = mkDefaultEnvAction ("" :: String),
                decryptFunc = resolveVar,
                logWhenThrowException = Nothing
              }

      result <- lookupEnvM env :: IO String
      result `shouldBe` "http://api.example.com/v1/users"
      cleanupEnv ["BASE_URL", "API_PATH", "FULL_PATH"]

  describe "Complex Type Conversions" $ do
    it "handles custom date-time format" $ do
      res <- getCurrentTime
      setupEnv [("TIMESTAMP", show res)]

      let parseDateTime :: String -> IO UTCTime
          parseDateTime = pure . read

          defaultTime :: UTCTime
          defaultTime = UTCTime (fromGregorian 1970 1 1) 0

          env :: JuspayEnv IO String UTCTime
          env = JuspayEnv "TIMESTAMP" (mkDefaultEnvAction defaultTime) parseDateTime Nothing

      result <- lookupEnvM env :: IO UTCTime
      result `shouldBe` res
      cleanupEnv ["TIMESTAMP"]

    it "parses complex vector type" $ do
      setupEnv [("VECTOR_DATA", "[1,2,3,4,5]")]

      let env :: JuspayEnv IO String (V.Vector Int)
          env =
            JuspayEnv
              { key = "VECTOR_DATA",
                actionLeft = mkDefaultEnvAction V.empty,
                decryptFunc = (\(x :: String) -> (either (error) (pure) . A.eitherDecode . encodeUtf8 . traceShowId) x),
                logWhenThrowException = Nothing
              }

      result <- lookupEnvM env :: IO (V.Vector Int)
      V.toList result `shouldBe` ([1, 2, 3, 4, 5] :: [Int])
      cleanupEnv ["VECTOR_DATA"]

  describe "Concurrent Access Tests" $ do
    it "handles multiple simultaneous lookups safely" $ do
      setupEnv [("CONCURRENT_VAR", "test-value")]

      let env :: JuspayEnv IO String String
          env = JuspayEnv "CONCURRENT_VAR" (mkDefaultEnvAction ("" :: String)) pure Nothing

      (result1, result2) <-
        concurrently
          (lookupEnvM env :: IO String)
          (lookupEnvM env :: IO String)
      result1 `shouldBe` ("test-value" :: String)
      result2 `shouldBe` ("test-value" :: String)
      cleanupEnv ["CONCURRENT_VAR"]

  describe "Advanced Error Handling" $ do
    it "handles chained error recovery" $ do
      setupEnv [("ERROR_CHAIN", "invalid-value")]

      let parseToInt :: String -> IO Int
          parseToInt s = case reads s of
            [(n, "")] -> pure n
            _ -> pure 42 -- Default to 42 on parse failure
          primaryEnv :: JuspayEnv IO String Int
          primaryEnv =
            JuspayEnv
              { key = "ERROR_CHAIN",
                actionLeft = mkDefaultEnvAction (42 :: Int),
                decryptFunc = parseToInt,
                logWhenThrowException = Nothing
              }

          backupEnv :: JuspayEnv IO String Int
          backupEnv =
            JuspayEnv
              { key = "BACKUP_VALUE",
                actionLeft = mkDefaultEnvAction (0 :: Int),
                decryptFunc = parseToInt,
                logWhenThrowException = Nothing
              }

      result1 <- lookupEnvM primaryEnv :: IO Int
      result2 <- lookupEnvM backupEnv :: IO Int
      (result1 == 42 || result2 == 0) `shouldBe` True
      cleanupEnv ["ERROR_CHAIN"]

  describe "Map Configuration Tests" $ do
    it "parses HashMap configuration" $ do
      let mapConfig :: String
          mapConfig = "{\"key1\":\"value1\",\"key2\":\"value2\"}"

      setupEnv [("MAP_CONFIG", mapConfig)]

      let env :: JuspayEnv IO String (HM.HashMap Text Text)
          env =
            JuspayEnv
              { key = "MAP_CONFIG",
                actionLeft = mkDefaultEnvAction HM.empty,
                decryptFunc = (\(x :: String) -> (either (error) (pure) . A.eitherDecode . encodeUtf8 . traceShowId) x),
                logWhenThrowException = Nothing
              }

      result <- lookupEnvM env :: IO (HM.HashMap Text Text)
      HM.lookup "key1" result `shouldBe` (Just "value1" :: Maybe Text)
      cleanupEnv ["MAP_CONFIG"]

  describe "Performance Edge Cases" $ do
    it "handles large environment variables" $ do
      let largeValue :: String
          largeValue = replicate 10000 'x'

      setupEnv [("LARGE_VAR", largeValue)]

      let env :: JuspayEnv IO String String
          env = JuspayEnv "LARGE_VAR" (mkDefaultEnvAction ("" :: String)) pure Nothing

      result <- lookupEnvM env :: IO String
      length result `shouldBe` (10000 :: Int)
      cleanupEnv ["LARGE_VAR"]

    it "processes deeply nested JSON" $ do
      let deepJson :: String
          deepJson = "{\"l1\":{\"l2\":{\"l3\":{\"l4\":{\"value\":42}}}}}"

      setupEnv [("DEEP_JSON", deepJson)]

      let parseJson :: String -> IO A.Value
          parseJson s = case A.eitherDecode (A.encode s) of
            Left _ -> pure A.Null
            Right v -> pure v

          env :: JuspayEnv IO String A.Value
          env =
            JuspayEnv
              { key = "DEEP_JSON",
                actionLeft = mkDefaultEnvAction A.Null,
                decryptFunc = parseJson,
                logWhenThrowException = Nothing
              }

      result <- lookupEnvM env :: IO A.Value
      result `shouldNotBe` A.Null
      cleanupEnv ["DEEP_JSON"]

  -- 19. Cryptographic Hash Tests
  describe "Cryptographic Hash Handling" $ do
    it "handles SHA256 hashed values" $ do
      let hashText = "hello"
          sha256Hash = show (Hash.hash (BS8.pack hashText) :: Hash.Digest Hash.SHA256)
      
      setupEnv [("HASH_VALUE", sha256Hash)]
      let env = JuspayEnv "HASH_VALUE" (mkDefaultEnvAction ("" :: String)) mockDecrypt Nothing
      result <- lookupEnvM env :: IO String
      result `shouldBe` sha256Hash
      cleanupEnv ["HASH_VALUE"]
  
    it "handles MD5 hashed values" $ do
      let hashText = "testdata"
          md5Hash = show (Hash.hash (BS8.pack hashText) :: Hash.Digest Hash.MD5)
      
      setupEnv [("MD5_VALUE", md5Hash)]
      let env = JuspayEnv "MD5_VALUE" (mkDefaultEnvAction ("" :: String)) mockDecrypt Nothing
      result <- lookupEnvM env :: IO String
      result `shouldBe` md5Hash
      cleanupEnv ["MD5_VALUE"]

    it "handles isJust Maybe" $ do
      let val = "HI"
      setupEnv [("TEST_AES_KEY", val)]
      let env = JuspayEnv "TEST_AES_KEY" (mkDefaultEnvAction (False :: Bool)) (\(x :: Maybe String) -> pure $ isJust x) Nothing
      result <- lookupEnvM env :: IO Bool
      result `shouldBe` True
      cleanupEnv ["TEST_AES_KEY"]
  
  -- 20. Base64 Encoding Tests
  describe "Base64 Encoding Tests" $ do
    it "handles Base64 encoded values" $ do
      let originalText = "Hello, World!"
          encodedText = BS8.unpack $ Base64.encode $ BS8.pack originalText
      
      setupEnv [("BASE64_VALUE", encodedText)]
      let decodeBase64 :: String -> IO String
          decodeBase64 str = case Base64.decode $ BS8.pack str of
            Left err -> error err
            Right decoded -> pure $ BS8.unpack decoded
      
      let env = JuspayEnv "BASE64_VALUE" (mkDefaultEnvAction ("" :: String)) decodeBase64 Nothing
      result <- lookupEnvM env :: IO String
      result `shouldBe` originalText
      cleanupEnv ["BASE64_VALUE"]
  
    it "handles invalid Base64 input" $ do
      setupEnv [("INVALID_BASE64", "not-base64!@#")]
      let decodeBase64 :: String -> IO String
          decodeBase64 str = case Base64.decode $ BS8.pack str of
            Left _ -> pure "default"
            Right decoded -> pure $ BS8.unpack decoded
      
      let env = JuspayEnv "INVALID_BASE64" (mkDefaultEnvAction ("default" :: String)) decodeBase64 Nothing
      result <- lookupEnvM env :: IO String
      result `shouldBe` "default"
      cleanupEnv ["INVALID_BASE64"]

  -- 21. AES Encryption Tests
  describe "AES Encryption Tests" $ do
    it "handles AES encrypted values" $ do
      let plaintext = "Plaintext Data for Encryption" :: Text
          key = "12345678901234561234567890123456" :: Text  -- 256-bit key
          iv = "1234567890123456" :: Text  -- 128-bit IV
          eCiphertext = EE.aesEncryptText key iv plaintext
      case eCiphertext of
        Left err -> print err
        Right ciphertext -> do
            setupEnv [("ENCRYPTED_VALUE", (T.unpack ciphertext))]
            let env = JuspayEnv "ENCRYPTED_VALUE" (mkDefaultEnvAction ("" :: Text)) (\x -> (either (error "Invalid decryptedtext") pure) $ EE.aesDecryptText key iv x) Nothing
            result <- lookupEnvM env :: IO Text
            result `shouldBe` plaintext
            cleanupEnv ["ENCRYPTED_VALUE"]