module Main where

import Criterion.Main
import Control.DeepSeq

import Reference

levSelf :: ([Int] -> [Int] -> Int) -> [Int] -> Int
levSelf lev nums = lev nums nums
                
oneToTen :: [[Int]]
oneToTen = map (\n -> [1..n]) [1..10]

main = oneToTen `deepseq` defaultMain [
  bgroup "refSelf" $ zipWith (levSelfBench levenshtein) [1..10] oneToTen
  ]
 where levSelfBench lev n nums = bench (show n) $ nf (levSelf lev) nums
