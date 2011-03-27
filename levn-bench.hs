module Main where

import Criterion.Main
import Control.DeepSeq
import qualified Data.Vector as V

import qualified Lev1 as Lev1
import qualified Lev2 as Lev2
import qualified Lev3 as Lev3
import qualified Lev4 as Lev4

levSelf :: Eq a => (V.Vector a -> V.Vector a -> Int) -> V.Vector a -> Int
levSelf lev as = lev as as
                
fiveHundred :: V.Vector Int
fiveHundred = let as = [1..500] in as `deepseq` V.fromList as

main = fiveHundred `seq` defaultMain [
  bgroup "lev1" $ [bench "500" $ nf (levSelf Lev1.lev') fiveHundred],
  bgroup "lev2" $ [bench "500" $ nf (levSelf Lev2.lev') fiveHundred],
  bgroup "lev3" $ [bench "500" $ nf (levSelf Lev3.lev') fiveHundred],
  bgroup "lev4" $ [bench "500" $ nf (levSelf Lev4.lev') fiveHundred]
  ]
