module Lev5 where

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

lev :: (Eq a) => [a] -> [a] -> Int
lev as bs = lev' (V.fromList as) (V.fromList bs)
            
-- | I can't believe I went to the trouble of computing diagonals in
-- Lev2, when all you really need is to compute rows. Sheeesh.
lev' :: (Eq a) => V.Vector a -> V.Vector a -> Int
lev' av bv = if w == 0 && h == 0 then 0 else firstRow U.! 0
  where w = V.length av
        h = V.length bv
        firstRow = fromBottomUp h lastRow
          where fromBottomUp 0 row = row
                fromBottomUp m row = fromBottomUp (m-1) $ prevRow m row
        lastRow = U.fromList $ take (w+1) $ iterate (\n -> n - 1) w
        prevRow y row =
          U.fromList rightToLeft
            where rightToLeft = accum w [1 + (row U.! w)]
                  accum 0 sofar = sofar 
                  accum n sofar = n' `seq` best `seq` accum n' (best:sofar)
                    where n' = n-1
                          best = min del (min ins subst)
                          del = 1 + head sofar
                          ins = 1 + (row U.! (n-1))
                          subst = (if (av V.! (n-1)) == bval then 0 else 1) + (row U.! n)
                  bval = bv V.! (y-1)
