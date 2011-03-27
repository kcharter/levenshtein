module Main where

import qualified Reference as Ref
import Test


main :: IO ()
main = do
  putStrLn "Test the reference implementation"
  test Ref.levenshtein

