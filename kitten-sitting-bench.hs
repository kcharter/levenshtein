module Main where

import Criterion.Main
import Control.DeepSeq
import qualified Data.Vector as V

import qualified Lev1 as Lev1
import qualified Lev2 as Lev2
import qualified Lev3 as Lev3
import qualified Lev4 as Lev4
import qualified Lev5 as Lev5

kitten :: V.Vector Char
kitten = repls 1 "kitten"

sitting :: V.Vector Char
sitting = repls 1 "sitting"

kitten30 :: V.Vector Char
kitten30 = repls 30 "kitten"

sitting30 :: V.Vector Char
sitting30 = repls 30 "sitting"

kitten300 :: V.Vector Char
kitten300 = repls 300 "kitten"

sitting300 :: V.Vector Char
sitting300 = repls 300 "sitting"

repls :: (NFData a) => Int -> [a] -> V.Vector a
repls n l = let l' = concat $ replicate n l in l' `deepseq` V.fromList l'

main = kitten `seq` sitting `seq` kitten30 `seq` sitting30 `seq` kitten300 `seq` sitting300 `seq` defaultMain [
  bgroup "kitten-sitting" $ [bench "Lev2 1" $ nf (Lev2.lev' kitten) sitting,
                             bench "Lev5 1" $ nf (Lev5.lev' kitten) sitting,
                             bench "Lev2 30" $ nf (Lev2.lev' kitten30) sitting30,
                             bench "Lev5 30" $ nf (Lev5.lev' kitten30) sitting30,
                             bench "Lev2 300" $ nf (Lev2.lev' kitten300) sitting300,
                             bench "Lev5 300" $ nf (Lev5.lev' kitten300) sitting300]
  ]
