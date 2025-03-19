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