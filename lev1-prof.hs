module Main where

import Lev1 (lev)

main :: IO ()
main = print $ lev [1..1000] [37..973]
