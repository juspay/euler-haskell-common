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

import Criterion.Main
import qualified Juspay.Extra.List as EList

main :: IO ()
main = do
  defaultMain
    [ bgroup "euler benchmark"
        [ bgroup "List extras"
          [ bench "shuffle 10 elements"     $ whnfIO $ EList.shuffle sampleArray10
          , bench "shuffle 100 elements"    $ whnfIO $ EList.shuffle sampleArray100
          , bench "shuffle 1000 elements"   $ whnfIO $ EList.shuffle sampleArray1000
          , bench "shuffle 10000 elements"  $ whnfIO $ EList.shuffle sampleArray10000
          ]
        ]
    ]
  where
    sampleArray10 :: [Integer]
    sampleArray10 = [1..10]

    sampleArray100 :: [Integer]
    sampleArray100 = [1..100]

    sampleArray1000 :: [Integer]
    sampleArray1000 = [1..1000]

    sampleArray10000 :: [Integer]
    sampleArray10000 = [1..10000]