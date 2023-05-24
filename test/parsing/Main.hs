{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE KindSignatures      #-}

module Main where


import           Data.Vector.NonEmpty (NonEmptyVector, unsafeFromList)
import           Control.Category ((>>>))
import           GHC.TypeLits (KnownSymbol)
import           Juspay.Extra.Parsing

import           Test.Hspec (describe, hspec, it, shouldBe)



main :: IO ()
main = hspec $ do
  describe "Parsing around" $ do
    it "around A" aroundMaybeListA
    it "around B" aroundMaybeListB
    it "correct parsing errors" checkErrors



-- Test data

data A = A
  { fieldA :: Maybe String
  }
  deriving (Eq, Show)

data RawA = RawA
  { rawFieldA :: Maybe Int
  }
  deriving (Eq, Show)


data B = B
  { fieldB1 :: Maybe String
  , fieldB2 :: String
  , fieldB3 :: String
  }
  deriving (Eq, Show)

data RawB = RawB
  { rawFieldB1 :: Maybe Int
  , rawFieldB2 :: Bool
  , rawFieldB3 :: Bool
  }
  deriving (Eq, Show)


-- We use this kind of parsers `around (liftPure foo)`
-- for wrapping some id fields which are not mandated
-- e.g. Maybe Int64 -> Maybe SomeInt64Id

aroundRawA :: Step ctx Int String
aroundRawA = liftPure show

-- working replacement for `around aroundRawA`
aroundRawA' :: Step ctx (Maybe Int) (Maybe String)
aroundRawA' = liftPure (fmap show)

parseA  :: RawA -> Parsed A
parseA rawType =
  A <$>
    go (project @"rawFieldA" >>> around aroundRawA)
  where
    go :: (KnownSymbol field) =>
      Step ('Just field) RawA b -> Parsed b
    go = parseField rawType

aroundRawB1 :: Step ctx Int String
aroundRawB1 = liftPure show

-- working replacement for `around aroundRawB1`
aroundRawB1' :: Step ctx (Maybe Int) (Maybe String)
aroundRawB1' = liftPure (fmap show)

rawB2mod ::  Step ctx Bool String
rawB2mod = liftEither go
  where
    go x = case x of
      True -> Right $ show x
      False -> Left $ Other "FALSE ERROR 111"

rawB2mod2 ::  Step ctx Bool String
rawB2mod2 = liftEither go
  where
    go x = case x of
      True -> Right $ show x
      False -> Left $ Other "FALSE ERROR 222"

parseB  :: RawB -> Parsed B
parseB rawType =
  B <$>
    go (project @"rawFieldB1" >>> around aroundRawB1) <*>
    go (project @"rawFieldB2" >>> rawB2mod) <*>
    go (project @"rawFieldB3" >>> rawB2mod2)
  where
    go :: (KnownSymbol field) =>
      Step ('Just field) RawB b -> Parsed b
    go = parseField rawType

--- A ---
nothingRawA :: RawA
nothingRawA = RawA Nothing

nothingA :: A
nothingA = A Nothing

justRawA :: RawA
justRawA = RawA (Just 12345)

justA :: A
justA = A (Just "12345")

rawJustAList :: [RawA]
rawJustAList = [justRawA, justRawA, justRawA]

justAList :: [A]
justAList = [justA, justA, justA]

rawNothingAList :: [RawA]
rawNothingAList = [nothingRawA, nothingRawA, nothingRawA]

nothingAList :: [A]
nothingAList = [nothingA, nothingA, nothingA]

rawMixedAList :: [RawA]
rawMixedAList = [justRawA, nothingRawA, justRawA, nothingRawA]

mixedAList :: [A]
mixedAList = [justA, nothingA, justA, nothingA]


--- B ---

nothingRawB :: RawB
nothingRawB = RawB Nothing True True

nothingB :: B
nothingB = B Nothing "True" "True"

justRawB :: RawB
justRawB = RawB (Just 12345) True True

justFalseRawB :: RawB
justFalseRawB = RawB (Just 12345) False False

justB :: B
justB = B (Just "12345") "True" "True"

rawJustBList :: [RawB]
rawJustBList = [justRawB, justRawB, justRawB]

justBList :: [B]
justBList = [justB, justB, justB]

rawNothingBList ::  [RawB]
rawNothingBList = [nothingRawB, nothingRawB, nothingRawB]

nothingBList :: [B]
nothingBList = [nothingB, nothingB, nothingB]

rawMixedBList :: [RawB]
rawMixedBList = [justRawB, nothingRawB, justRawB, nothingRawB]

rawMixedFailBList :: [RawB]
rawMixedFailBList = [justFalseRawB, nothingRawB, justFalseRawB, nothingRawB]

mixedBList :: [B]
mixedBList = [justB, nothingB, justB, nothingB]

parsingErr :: NonEmptyVector ParsingError
parsingErr = unsafeFromList
  [ RecordParsingError
      { recordType = "RawB"
      , fieldName = "rawFieldB2"
      , errorType = Other {message = "FALSE ERROR 111"}
      }
  , RecordParsingError
      { recordType = "RawB"
      , fieldName = "rawFieldB3"
      , errorType = Other {message = "FALSE ERROR 222"}
      }
  , RecordParsingError
      { recordType = "RawB"
      , fieldName = "rawFieldB2"
      , errorType = Other {message = "FALSE ERROR 111"}
      }
  , RecordParsingError
      { recordType = "RawB"
      , fieldName = "rawFieldB3"
      , errorType = Other {message = "FALSE ERROR 222"}
      }
  ]
---helpers


-- commented cases can't finish parsing and just trying to consume all memory
-- possible reason - traverse (fmap (+1)) (Nothing :: Maybe (ZipList Int))
-- produces infinite  list with Nothing
aroundMaybeListA :: IO ()
aroundMaybeListA = do
  let parsingJustOnly = traverse parseA rawJustAList
  let parsingNothingOnly = traverse parseA rawNothingAList
  let parsingMixed = traverse parseA rawMixedAList
  case parsingJustOnly of
    Result jv -> do
      jv `shouldBe` justAList
    Failed e -> fail $ "Unexpected failure: " <> show e
  case parsingNothingOnly of
    Result nv -> do
      nv `shouldBe` nothingAList
    Failed e -> do
      fail $ "Unexpected failure: " <> show e
  case parsingMixed of
    Result mv -> do
      mv `shouldBe` mixedAList
    Failed e -> fail $ "Unexpected failure: " <> show e

aroundMaybeListB :: IO ()
aroundMaybeListB = do
  let parsingJustOnly = traverse parseB rawJustBList
  let parsingNothingOnly = traverse parseB rawNothingBList
  let parsingMixed = traverse parseB rawMixedBList
  case parsingJustOnly of
    Result jv -> do
      jv `shouldBe` justBList
    Failed e -> fail $ "Unexpected failure: " <> show e
  case parsingNothingOnly of
    Result nv -> do
      nv `shouldBe` nothingBList
    Failed e -> do
      fail $ "Unexpected failure: " <> show e
  case parsingMixed of
    Result mv -> do
      mv `shouldBe` mixedBList
    Failed e -> fail $ "Unexpected failure: " <> show e



checkErrors:: IO ()
checkErrors = do
  let parsingFailed = traverse parseB rawMixedFailBList
  case parsingFailed of
    Failed err -> do
      err `shouldBe` parsingErr
    Result er -> fail $ "Unexpected success: " <> show er
