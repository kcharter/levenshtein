module Main where

import Lev2 (lev)

main :: IO ()
main = print $ lev [1..1000] [37..973]
