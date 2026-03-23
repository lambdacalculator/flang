{-# LANGUAGE TypeFamilies, DeriveFunctor, DeriveFoldable, DeriveGeneric, DeriveAnyClass #-}
-- |
-- Module      : FLang.RegExp
-- Description : Regular Expressions with ACUI normalization
-- Copyright   : (c) Todd Wilson, 2026
-- License     : BSD-3-Clause
--
-- This module defines the 'RegExp' data type.
--
-- Note for Students:
-- This module provides the smart Brzozowski derivative '(<\>)', 
-- which applies ACUI normalization to ensure a finite set of 
-- derivatives for any regular expression.

module FLang.RegExp 
  ( RegExp(..)
  , Compact(..)
  , toRE
  , lang_of
  , onestr
  , finite
  , byp
  , (<\>)
  , mkUnion, (<+>)
  , mkCat, (<.>)
  , mkStar
  , match1
  , match2
  , match3
  , MProg(..)
  , runmp
  , compile

  ) where

import FLang.LOL (eps)
import FLang.Language hiding (strings)
import FLang.Utils (splits)

import Data.Reify
import FLang.CyclicShow (CyclicPrint(..), cyclicShow)
import GHC.Generics (Generic1)

-- | Standard Regular Expressions.
data RegExp = Zero                 -- ^ Empty language
            | One                  -- ^ Empty string language
            | Let Char             -- ^ Single letter language
            | Union RegExp RegExp  -- ^ Union
            | Cat RegExp RegExp    -- ^ Concatenation
            | Star RegExp          -- ^ Kleene star
            deriving (Show, Eq, Ord)

-- Compact display form for RegExp
newtype Compact = Compact RegExp

instance Show Compact where    -- use precedence to minimize parentheses
  showsPrec p (Compact r) = sp p r where
    sp _ Zero          = showString "0"
    sp _ One           = showString "1"
    sp _ (Let c)       = showChar c
    sp d (Union r1 r2) = showParen (d > 6) $  -- prec(Union) = 6
                         sp 6 r1 .
                         showString "+" .
                         sp 6 r2
    sp d (Cat r1 r2)   = showParen (d > 7) $  -- prec(Cat) = 7
                         sp 7 r1 .
                         sp 7 r2
    sp _ (Star r1)     = sp 9 r1 .     -- prec(Star) = 8
                         showString "*"


-- Quick and dirty postfix RegExp parser, with error messages. Skips spaces.
toRE :: String -> RegExp
toRE w = go w [] where
  ----------- Base case -------------
  go [] [r]              = r     -- Normal termination: one result on stack
  ----------- Error cases -----------
  go [] []        = error "Empty input"
  go ('+':_) []  = error "No operands for +"
  go ('.':_) []  = error "No operands for ."
  go ('*':_) []  = error "No operand for *"
  go ('+':_) [r] = error $ "Missing operand for + (other operand: " ++
                    show (Compact r) ++ ")"
  go ('.':_) [r] = error $ "Missing operand for . (other operand: " ++
                    show (Compact r) ++ ")"
  go [] rs        = error $ "Dangling operands: " ++
                    show (map Compact $ reverse rs)
  ----------- Normal cases ----------
  go (' ':xs) rs         = go xs rs           -- skip spaces
  go ('+':xs) (r2:r1:rs) = go xs (Union r1 r2:rs)
  go ('.':xs) (r2:r1:rs) = go xs (Cat r1 r2:rs)
  go ('*':xs) (r:rs)     = go xs (Star r:rs)
  go ('0':xs) rs         = go xs (Zero:rs)
  go ('1':xs) rs         = go xs (One:rs)
  go (x:xs) rs           = go xs (Let x:rs)   -- everything else is a letter

--------------------------------------------------------------------------------
-- Smart Constructors (ACUI Normalization)
--------------------------------------------------------------------------------

-- | Infix alias for Smart Union
(<+>) :: RegExp -> RegExp -> RegExp
(<+>) = mkUnion

-- | Infix alias for Smart Concatenation
(<.>) :: RegExp -> RegExp -> RegExp
(<.>) = mkCat

-- | Smart Constructor for Union.
-- Enforces:
-- 1. Right-Associativity: (A+B)+C -> A+(B+C)
-- 2. Deep Idempotence: Checks if 'p' exists anywhere in 'q'
-- 3. Identity: Removes Zero
mkUnion :: RegExp -> RegExp -> RegExp
mkUnion Zero q = q
mkUnion p Zero = p
mkUnion p q = case p of
    Union a b -> mkUnion a (mkUnion b q)
    _         -> insert p q

-- | Helper: Inserts 'p' into 'q' only if not physically present in the spine.
-- This guarantees A+B+A -> A+B without requiring a full sort.
insert :: RegExp -> RegExp -> RegExp
insert p q =
  if p == q then q
  else case q of
    Union a b ->
      if p == a then q            -- Found match at head
      else Union a (insert p b)   -- Recurse down the spine
    _ -> Union p q                -- No match found, prepend

-- | Smart Constructor for Concatenation.
-- Enforces:
-- 1. Right-Associativity: (AB)C -> A(BC)
-- 2. Identity: One.x -> x, x.One -> x
-- 3. Annihilation: Zero.x -> Zero, x.Zero -> Zero
mkCat :: RegExp -> RegExp -> RegExp
mkCat Zero _ = Zero
mkCat _ Zero = Zero
mkCat One q  = q
mkCat p One  = p
mkCat p q =
  case p of
    Cat a b -> mkCat a (mkCat b q) -- Right-associate
    _       -> Cat p q

-- | Smart Constructor for Star.
mkStar :: RegExp -> RegExp
mkStar Zero     = One
mkStar One      = One
mkStar (Star r) = Star r -- Idempotence: (r*)* -> r*
mkStar r        = Star r



--------------------------------------------------------------------------------
-- Denotation and Helpers
--------------------------------------------------------------------------------

-- The language associated to (aka denotation of) a regular expression: [[r]]
lang_of :: RegExp -> Lang Char
lang_of Zero = []
lang_of One = [eps]
lang_of (Let a) = lang [[a]]
lang_of (Union r1 r2) = merge (lang_of r1) (lang_of r2)
lang_of (Cat r1 r2) = cat (lang_of r1) (lang_of r2)
lang_of (Star r1) = kstar (lang_of r1)


-- The one-string and finite languages of Theorem 3.2.
onestr :: String -> RegExp
onestr [] = One
onestr [x] = Let x
onestr (x:xs) = Cat (Let x) (onestr xs)

finite :: [String] -> RegExp
finite [] = Zero
finite [w] = onestr w
finite (w:ws) = Union (onestr w) (finite ws)


---- Some recursive functions on regular expressions

-- | Compute bypassability (nullable)
byp :: RegExp -> Bool
byp Zero = False
byp One = True
byp (Let _) = False
byp (Union r1 r2) = byp r1 || byp r2
byp (Cat r1 r2) = byp r1 && byp r2
byp (Star _) = True

-- | Smart Left Quotient (Brzozowski Derivative).
-- Uses smart constructors (<+>, <.>) to apply ACUI normalization, guaranteeing
-- a finite set of derivatives for any regular expression.
(<\>) :: Char -> RegExp -> RegExp
_ <\> Zero        = Zero
_ <\> One         = Zero
c <\> (Let x)     = if c == x then One else Zero
c <\> (Union p q) = (c <\> p) <+> (c <\> q)
c <\> (Cat p q)   = ((c <\> p) <.> q) <+> (if byp p then (c <\> q) else Zero)
c <\> (Star r)    = (c <\> r) <.> mkStar r


-- | Matching algorithm 1: Trying splits.
match1 :: RegExp -> String -> Bool
match1 Zero _ = False
match1 One w = null w
match1 (Let c) w = w == [c]
match1 (Union r1 r2) w = match1 r1 w || match1 r2 w
match1 (Cat r1 r2) w = any (\(w1,w2) -> match1 r1 w1 && match1 r2 w2) (splits w)
match1 (Star r) w = null w ||
  any (\(w1,w2) -> match1 r w1 && match1 (Star r) w2) (tail $ splits w)

-- | Matching algorithm 2: Sequencing matches.
match2 :: RegExp -> String -> Bool
match2 r w = m0 [r] w where
  -- m0 matches a list of regexes in sequence, with no restrictions
  m0 :: [RegExp] -> String -> Bool
  m0 [] cs = null cs
  m0 (Zero : _) _ = False
  m0 (One : rs) cs = m0 rs cs
  m0 (Let a : rs) (x:xs) = a == x && m0 rs xs
  m0 (Let _ : _) [] = False
  m0 (Union r1 r2 : rs) cs = m0 (r1:rs) cs || m0 (r2:rs) cs
  m0 (Cat r1 r2 : rs) cs = m0 (r1:r2:rs) cs
  m0 (Star r1 : rs) cs = m0 rs cs || m1 (r1:Star r1:rs) cs

  -- same, but the first regex must match a non-empty string
  m1 :: [RegExp] -> String -> Bool
  m1 [] _ = False
  m1 (Zero : _) _ = False
  m1 (One : _) _ = False
  m1 (Let a : rs) (x:xs) = a == x && m0 rs xs
  m1 (Let _ : _) [] = False
  m1 (Union r1 r2 : rs) cs = m1 (r1:rs) cs || m1 (r2:rs) cs
  m1 (Cat r1 r2 : rs) cs = m1 (r1:r2:rs) cs || byp r1 && m1 (r2:rs) cs
  m1 (Star r1 : rs) cs = m1 (r1:Star r1:rs) cs

-- | Matching algorithm 3: Circular matching programs
-- When matching multiple strings, regular expressions should be
-- compiled once and then run on each string using runmp.
match3 :: RegExp -> String -> Bool
match3 r w = runmp (compile r) w

-- Matching programs and CyclicShow infrastructure
data MProg = Epsilon          -- Match the empty string
           | Fail             -- Match failure
           | Read Char MProg  -- Read a specific char then continue matching
           | Try MProg MProg  -- Try two matches in order (short-circuiting)

data MProgF e = EpsilonF
              | FailF
              | ReadF Char e
              | TryF e e
              deriving (Functor, Foldable, Generic1, CyclicPrint)

instance MuRef MProg where
  type DeRef MProg = MProgF
  mapDeRef _ Epsilon = pure EpsilonF
  mapDeRef _ Fail = pure FailF
  mapDeRef f (Read c next) = ReadF c <$> f next
  mapDeRef f (Try p1 p2) = TryF <$> f p1 <*> f p2

instance Show MProg where
  show = cyclicShow

-- | Run a matching program on a string
runmp :: MProg -> String -> Bool
runmp Epsilon w = null w
runmp Fail _ = False
runmp (Read _ _) [] = False
runmp (Read a mp) (x:xs) = a == x && runmp mp xs
runmp (Try mp1 mp2) w = runmp mp1 w || runmp mp2 w

-- | Compile a regular expression to a matching program
compile :: RegExp -> MProg
compile r = mpE where
  (_, mpE, _) = go r Epsilon

  -- go r mp returns (byp r, mp0, mp1) where
  -- mp0 matches r then continues with mp (r could match 0 chars)
  -- mp1 matches r then continues with mp (r must match at least 1 char)
  go :: RegExp -> MProg -> (Bool, MProg, MProg)
  go Zero _ = (False, Fail, Fail)
  go One mp = (True, mp, Fail)
  go (Let a) mp = (False, node, node) where node = Read a mp
  go (Union r1 r2) mp = (b1 || b2, Try mp10 mp20, Try mp11 mp21) where
    (b1, mp10, mp11) = go r1 mp
    (b2, mp20, mp21) = go r2 mp
  go (Cat r1 r2) mp = (b1 && b2, mp10, if b1 then Try mp11 mp21 else mp11) where
    (b2, mp20, mp21) = go r2 mp
    (b1, mp10, mp11) = go r1 mp20
  go (Star r1) mp = (True, m0, m1) where
    m0 = Try mp m1
    (_, _, m1) = go r1 m0
