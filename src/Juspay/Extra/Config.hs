module Juspay.Extra.Config
  (
    lookupEnv
  , lookupEnvS
  , lookupEnvT
  ) where

import           Data.Bifunctor          (bimap)
import           Data.Map.Strict         (Map)
import qualified Data.Map.Strict         as Map
import           Data.String.Conversions (ConvertibleStrings, convertString)
import           Data.Text               (Text, pack)
import           System.Environment      (getEnvironment)
import           System.IO.Unsafe        (unsafePerformIO)

-- | Gets environment variables once
environmentVars :: Map Text Text
environmentVars = Map.fromList $ bimap pack pack <$> unsafePerformIO getEnvironment
{-# NOINLINE environmentVars #-}

-- | Find a particular variable value by its name.
lookupEnv
  :: ConvertibleStrings name Text
  => ConvertibleStrings Text value
  => name -> Maybe value
lookupEnv k = convertString <$> Map.lookup (convertString k) environmentVars

-- | Shortcut for 'lookupEnv' @String
lookupEnvS :: ConvertibleStrings Text value => String -> Maybe value
lookupEnvS = lookupEnv

-- | Shortcut for 'lookupEnv' @Text
lookupEnvT :: ConvertibleStrings Text value => Text -> Maybe value
lookupEnvT = lookupEnv
