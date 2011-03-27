module Lev3 where

import Control.Monad (liftM)
import Control.Monad.ST
import Data.Maybe (fromJust)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed.Mutable as M

import Grid

lev :: (Eq a) => [a] -> [a] -> Int
lev as bs = lev' (V.fromList as) (V.fromList bs)
            
-- | In this version of the Levenshtein distance, we allocate three
-- mutable unboxed vectors for computing diagonals, and use them in
-- the ST monad, rather than allocating a completely new vector for
-- each diagonal as we compute it.
--
-- I had expected that this would produce a performance improvement,
-- but so far I haven't seen one. The best I can do is a little less
-- than half the speed of the Lev2 version, and that's using
-- 'unsafeRead' and 'unsafeWrite'.
lev' :: (Eq a) => V.Vector a -> V.Vector a -> Int
lev' av bv =
  let w = V.length av
      h = V.length bv
      nextDiag p@(x,y) diag1 diag2 diag3 =
        {-# SCC "nextDiag" #-} maybe (return ()) nextDiag' (gridToDiag w h p)
        where nextDiag' n =
                lev''' p diag1 diag2 >>= M.unsafeWrite diag3 n >> nextDiag (x+1,y-1) diag1 diag2 diag3
        --mapM_ (\p -> lev''' p diag1 diag2 >>= M.write diag3 (fromJust (gridToDiag w h p))) $ diagPath w h (x,y)
      lev''' (x,y) diag1 diag2 =
        let fromDiag d = {-# SCC "fromDiag"#-} maybe (return Nothing) (liftM Just . M.unsafeRead d) . gridToDiag w h
            inc1 = liftM $ fmap (1+)
            inc2 = liftM $ fmap ((if x < w && y < h && (av V.! x) == (bv V.! y) then 0 else 1)+)
            mm mx my = maybe my (\x -> Just $ maybe x (min x) my) mx
        in {-# SCC "lev3" #-} do
          del <- inc1 $ fromDiag diag1 (x+1,y)
          ins <- inc1 $ fromDiag diag1 (x,y+1)
          subst <- inc2 $ fromDiag diag2 (x+1,y+1)
          return $ fromJust $ mm del (mm ins subst)
      lev'' :: (Int,Int) -> M.MVector s Int -> M.MVector s Int -> M.MVector s Int -> ST s Int
      lev'' p@(x,y) diag1 diag2 diag3 =
        if x == 0 && y == 0
        then lev''' p diag1 diag2
        else {-# SCC "lev2_2" #-} nextDiag p diag1 diag2 diag3 >> lev'' p' diag3 diag1 diag2
          where p' = prevLeftBottom p
      prevLeftBottom (x,y) = if x > 0 then (x-1,y) else (x, max 0 (y-1))
  in if w == 0 && h == 0
     then 0
     else let (w',h') = prevLeftBottom (w,h)
              diagLen = min (w+1) (h+1)
          in runST $ do
            diag1 <- M.new diagLen
            diag2 <- M.new diagLen
            diag3 <- M.new diagLen
            M.write diag1 0 0
            lev'' (w',h') diag1 diag2 diag3

