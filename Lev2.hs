module Lev2 where

import Data.Maybe (fromJust)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

import Grid

lev :: (Eq a) => [a] -> [a] -> Int
lev as bs = lev' (V.fromList as) (V.fromList bs)
            
-- | This is like the Levenshtein distance function in 'Lev1', except
-- that we avoid creating a list, using @catMaybes@, and then using
-- @minimum@. Instead, we define a function @mm@ for finding the
-- minimum of two @Maybe Int@ values. Profiling suggested this might
-- provide an easy speed-up, and Criterion confirms that when
-- computing the (zero) distance between a 500-integer vector and
-- itself, the change speeds things up by about 30 percent.
lev' :: (Eq a) => V.Vector a -> V.Vector a -> Int
lev' av bv =
  let w = V.length av
      h = V.length bv
      nextDiag (x,y) diag1 diag2 =
        ({-# SCC "nextDiag" #-} U.fromList $ map (\p -> lev''' p diag1 diag2) $ diagPath w h (x,y))
      lev''' (x,y) diag1 diag2 =
        let fromDiag d = fmap (d U.!) . gridToDiag w h
            inc1 = fmap (1+)
            inc2 = fmap ((if x < w && y < h && (av V.! x) == (bv V.! y) then 0 else 1)+)
            mm mx my = maybe my (\x -> Just $ maybe x (min x) my) mx
        in {-# SCC "lev3" #-} fromJust $
                              mm (inc1 $ fromDiag diag1 (x+1, y)) $
                              mm (inc1 $ fromDiag diag1 (x, y+1)) (inc2 $ fromDiag diag2 (x+1, y+1))
      lev'' (0,0) diag1 diag2 = {-# SCC "lev2_1" #-} lev''' (0,0) diag1 diag2
      lev'' (x,y) diag1 diag2 = {-# SCC "lev2_2" #-} lev'' (x',y') diag1' diag2'
        where (x',y') = prevLeftBottom (x,y)
              diag1' = nextDiag (x,y) diag1 diag2
              diag2' = diag1
      prevLeftBottom (x,y) = if x > 0 then (x-1,y) else (x, max 0 (y-1))
  in if w == 0 && h == 0
     then 0
     else let (w',h') = prevLeftBottom (w,h)
          in lev'' (w',h') (U.fromList [0]) (U.fromList [])

