module Test where

import Test.QuickCheck

import qualified Reference as Ref

type Lev a = [a] -> [a] -> Int

test :: Lev Int -> IO ()
test lev = do
  quickCheck (forAll2 small small (prop_isLev lev))
  quickCheck (forAll small $ prop_levSelf lev)
  quickCheck (forAll2 small small (prop_levCommutes lev))
  quickCheck (forAll2 small small (prop_levReverse lev))
              
small :: Gen [Int]
small = resize 6 $ listOf arbitrary

prop_levSelf :: Lev Int -> [Int] -> Bool
prop_levSelf lev as = lev as as == 0

prop_levCommutes :: Lev Int -> [Int] -> [Int] -> Bool
prop_levCommutes lev as bs = lev as bs == lev bs as

prop_levReverse :: Lev Int -> [Int] -> [Int] -> Bool
prop_levReverse lev as bs = lev (reverse as) (reverse bs) == lev as bs

-- | Tests that a Levenshtein distance function is the same as the
-- reference implementation. Don't use this with anything but really
-- small inputs.
prop_isLev :: Lev Int -> [Int] -> [Int] -> Bool
prop_isLev lev as bs = Ref.levenshtein as bs == lev as bs

forAll2 :: (Testable prop, Show a, Show b) => Gen a -> Gen b -> (a -> b -> prop) -> Property
forAll2 ga gb f = forAll ga $ forAll gb . f

