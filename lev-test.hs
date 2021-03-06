module Main where

import qualified Reference as Ref
import qualified Lev1 as Lev1
import qualified Lev2 as Lev2
import qualified Lev3 as Lev3
import qualified Lev4 as Lev4
import qualified Lev5 as Lev5
import Test


main :: IO ()
main = do
  putStrLn "Test the reference implementation"
  test Ref.levenshtein
  putStrLn "Test the Lev1 implementation"
  test Lev1.lev
  putStrLn "Test the Lev2 implementation"
  test Lev2.lev
  putStrLn "Test the Lev3 implementation"
  test Lev3.lev
  putStrLn "Test the Lev4 implementation"
  test Lev4.lev
  putStrLn "Test the Lev5 implementation"
  test Lev5.lev

