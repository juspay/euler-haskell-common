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

{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Juspay.Extra.List
  ( shuffle
  )
where

import           Prelude
import qualified System.Random as R
import qualified Data.List as DL

shuffle :: [a] -> IO [a]
shuffle []  = pure []
shuffle [x] = pure [x]
shuffle xs  = do
  n <- R.getStdRandom $ R.randomR (0, DL.length xs - 1)
  let (front, rear)   = splitAt n xs
      (val, tailVals) = maybe ([], []) (\(a, as) -> ([a], as)) $ DL.uncons rear
  shuffledTail <- shuffle (front <> tailVals)
  pure $ val <> shuffledTail