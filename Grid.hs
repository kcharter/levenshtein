module Grid where

gridToDiag :: Int -> Int -> (Int,Int) -> Maybe Int
gridToDiag w h (x,y) = if inGrid w h (x,y) then Just (x - x') else Nothing
  where x' = max 0 (x - (h-y))

diagPath :: Int -> Int -> (Int, Int) -> [(Int,Int)]
diagPath w h (x,y) = takeWhile (inGrid w h) $ iterate rightAndUp (x,y)
  where rightAndUp (x,y) = (x+1, y-1)

inGrid :: Int -> Int -> (Int,Int) -> Bool
inGrid maxx maxy (x, y) = inRange maxx x && inRange maxy y

inRange :: Int -> Int -> Bool                          
inRange max n = n >= 0 && n <= max

