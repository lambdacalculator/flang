module FLang.Utils where

import Data.List (sort, inits, tails)

---------------- General list functions ---------------------------------------

-- Set difference
diff :: Eq a => [a] -> [a] -> [a]
diff xs ys = filter (\x -> notElem x ys) xs

-- Convert equivalence relation on a set to a partition
eq2part :: Eq a => [a] -> [(a,a)] -> [[a]]
eq2part as rs = blocks as where
  blocks [] = []
  blocks (a:rest) = let b = eqclass a
                  in b : blocks (rest `diff` b)
  eqclass x = [y | (x',y) <- rs, x == x']


-- Normalize a list: sort and remove duplicates
norm :: Ord a => [a] -> [a]
norm xs = rad $ sort xs where
  rad :: Eq a => [a] -> [a]  -- Remove adjacent duplicates
  rad [] = []
  rad [x] = [x]
  rad (x:ys@(y:_)) | x == y = rad ys
                    | otherwise = x : rad ys

-- Cartesian product, preserves normalization
cart :: [a] -> [b] -> [(a,b)]
cart xs ys = [(x,y) | x <- xs, y <- ys]

-- Powerset, preserves normalization
power :: [a] -> [[a]]
power [] = [[]]
power (x:xs) = let ys = power xs
               in [] : map (x:) ys ++ tail ys

-- Check whether two normalized lists overlap
overlap :: Ord a => [a] -> [a] -> Bool
overlap [] _ = False
overlap _ [] = False
overlap xs@(x:xr) ys@(y:yr) = case compare x y of
                                LT -> overlap xr ys
                                EQ -> True
                                GT -> overlap xs yr

-- splits xs = list of all possible splits of xs, in order. For example,
-- splits "abc" = [("","abc"), ("a","bc"), ("ab","c"), ("abc","")]
splits :: [a] -> [([a], [a])]
splits xs = zip (inits xs) (tails xs)

-- Unary set closure (where "set" = normalized list)
-- uclosure xs g == smallest set containing xs and closed under g
uclosure :: Ord a => [a] -> (a -> [a]) -> [a]
uclosure xs g = sort $ stable $ iterate close (xs, []) where
  stable ((fr,qs):rest) = if null fr then qs else stable rest
  stable [] = error "uclosure: empty list" -- should be impossible with iterate
  close (fr, qs) = (fr', qs') where
    qs' = fr ++ qs
    fr' = norm $ filter (`notElem` qs') $ concatMap g fr
