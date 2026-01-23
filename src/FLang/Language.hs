module FLang.Language where

import FLang.Utils (norm)
import FLang.LOL

---------------- Languages ----------------------------------------------------

-- | Type for the alphabet {a, b}


-- | Languages over an alphabet 'a', represented as normalized lists of LOLs.
--
-- Invariant: @xs :: Lang a@ implies @xs@ is sorted with no duplicates
type Lang a = [LOL a]

-- Smart constructor for (finite) languages, establishes invariant
lang :: Ord a => [[a]] -> Lang a
lang xs = norm $ map lol xs

-- Membership for languages (infinite lists satisfying invariant included)
memb :: Ord a => LOL a -> Lang a -> Bool
memb _ [] = False
memb x (y:ys) =
  case compare x y of
    LT -> False
    EQ -> True
    GT -> memb x ys
                           
-- Union of languages, preserving invariant (just a no-dups merge of lists)
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge xs@(x:xr) ys@(y:yr) =
  case compare x y of
    LT -> x : merge xr ys
    EQ -> x : merge xr yr
    GT -> y : merge xs yr

-- Concatenation of languages, preserving invariant
cat :: Ord a => Lang a -> Lang a -> Lang a
cat [] _ = []
cat _ [] = []
cat (x:xr) ys@(y:yr) = dot x y : merge (map (dot x) yr) (cat xr ys)

-- Kleene star of a language, preserving invariant
kstar :: Ord a => Lang a -> Lang a
kstar [] = [eps]
kstar (LOL 0 []:xr) = kstar xr
kstar xs = eps : cat xs (kstar xs)


-- All strings of length <= n over sigma, in LOL order. Useful for testing
strings :: [s] -> Int -> [[s]]
strings sigma n = concat [strs i | i <- [0..n]] where
  strs 0 = [[]]
  strs k = [a:xs | a <- sigma, xs <- strs (k-1)]
