module FLang.LOL where

---------------- Length-ordered lists -----------------------------------------

-- | Type of Length-Ordered Lists (LOLs).
--
-- Invariant: In @LOL n xs@, @n == length xs@.
data LOL a = LOL !Int [a] deriving (Eq,Ord)

-- Smart constructor for LOL a, establishes invariant
lol :: [a] -> LOL a
lol xs = LOL (length xs) xs

-- Destructor for LOL a (ignores length)
unlol :: LOL a -> [a]
unlol (LOL _ xs) = xs

-- Show instance doesn't print LOL constructor or length
instance Show a => Show (LOL a) where
  show = show . unlol

-- Empty list (epsilon)
eps :: LOL a
eps = LOL 0 []

-- Concatenation of LOLs, preserves invariant
dot :: LOL a -> LOL a -> LOL a
dot xs (LOL 0 _) = xs                            -- special case, for efficiency
dot (LOL n xs) (LOL m ys) = LOL (n+m) (xs++ys)   -- general case

-- Reverse of an LOL, preserves invariant
rev :: LOL a -> LOL a
rev (LOL n xs) = LOL n (reverse xs)
