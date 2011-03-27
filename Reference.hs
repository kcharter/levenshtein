module Reference where

levenshtein :: (Eq a) => [a] -> [a] -> Int
levenshtein [] s2 = length s2
levenshtein s1 [] = length s1
levenshtein (a:as) (b:bs) = minimum [deletion, insertion, substitution]
  where deletion = 1 + levenshtein as (b:bs)
        insertion = 1 + levenshtein (a:as) bs
        substitution = (if a == b then 0 else 1) + levenshtein as bs

