-- |
-- Module      : FLang.RegExp
-- Description : Regular Expressions with ACUI normalization
-- Copyright   : (c) Todd Wilson, 2026
-- License     : BSD-3-Clause
--
-- This module defines the 'RegExp' data type.
--
-- Note for Students:
-- This module provides two types of derivatives:
--
-- 1. 'lq': A naive/pedagogical left quotient that preserves structure.
--    Use this to explore why state explosion occurs (e.g. on @(a*)*@).
--
-- 2. '(<\>)': A smart Brzozowski derivative that applies ACUI normalization.
--    Use this for algorithms like 'brzozowski' to ensure termination.

module FLang.RegExp 
  ( RegExp(..)
  , Compact(..)
  , toRE
  , lang_of
  , onestr
  , finite
  , bypnep
  , byp
  , nep
  , lq
  , (<\>)
  , mkUnion, (<+>)
  , mkCat, (<.>)
  , mkStar
  , numLets
  , starHgt
  , match1
  , match2
  , MProg(..)
  , runmp
  , compile
  , match3
  , reverseRE

  ) where

import FLang.LOL (eps)
import FLang.Language hiding (strings)
import FLang.Utils (splits)

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

-- | Simultaneous computation of bypassability (nullable) and non-empty part.
-- Returns 'Just r'' if the regex is nullable (accepts epsilon), where r' is the "non-empty part".
bypnep :: RegExp -> Maybe RegExp
bypnep Zero = Nothing
bypnep One = Just Zero -- nep(1) = 0 because 1 = 1 + 0
bypnep (Let _) = Nothing
bypnep (Union r1 r2) = case (bypnep r1, bypnep r2) of
                         (Nothing,  Nothing)  -> Nothing
                         (Nothing,  Just r2') -> Just $ Union r1 r2'
                         (Just r1', Nothing)  -> Just $ Union r1' r2
                         (Just r1', Just r2') -> Just $ Union r1' r2'
bypnep (Cat r1 r2) = case (bypnep r1, bypnep r2) of
                       (Just r1', Just r2') -> Just $ Union r1' (Cat r1 r2')
                       _ -> Nothing
bypnep r@(Star r1) = Just $ Cat (nep r1) r

-- We can recover byp and nep as thin wrappers on bypnep
byp :: RegExp -> Bool
byp r = case bypnep r of
          Nothing -> False
          Just _ -> True

nep :: RegExp -> RegExp
nep r = case bypnep r of
          Nothing -> r
          Just r' -> r'

-- | Naive Left Quotient (by a letter).
-- This preserves the existing structure as much as possible, for pedagogical demonstration.
-- Warning: repeated application on terms like (a*)* may lead to infinite growth.
lq :: Char -> RegExp -> RegExp
lq _ Zero = Zero
lq _ One = Zero
lq s (Let c) = if s == c then One else Zero
lq s (Union r1 r2) = Union (lq s r1) (lq s r2)
lq s (Cat r1 r2) | byp r1 = Union (Cat (lq s r1) r2) (lq s r2)
                 | otherwise = Cat (lq s r1) r2
lq s (Star r1) = Cat (lq s r1) (Star r1)

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


-- Number of letters 
numLets :: RegExp -> Int
numLets Zero = 0
numLets One = 0
numLets (Let _) = 1
numLets (Union r1 r2) = numLets r1 + numLets r2
numLets (Cat r1 r2) = numLets r1 + numLets r2
numLets (Star r1) = numLets r1

-- Star height (depth of nested stars)
starHgt :: RegExp -> Int
starHgt Zero = 0
starHgt One = 0
starHgt (Let _) = 0
starHgt (Union r1 r2) = starHgt r1 `max` starHgt r2
starHgt (Cat r1 r2) = starHgt r1 `max` starHgt r2
starHgt (Star r1) = 1 + starHgt r1

-- Reversal of a Regular Expression
reverseRE :: RegExp -> RegExp
reverseRE Zero = Zero
reverseRE One = One
reverseRE (Let c) = Let c
reverseRE (Union r1 r2) = Union (reverseRE r1) (reverseRE r2)
reverseRE (Cat r1 r2) = Cat (reverseRE r2) (reverseRE r1) -- Note order swap
reverseRE (Star r) = Star (reverseRE r)

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
match2 r w = m [r] False w where
  m :: [RegExp] -> Bool -> String -> Bool  -- for c, False is 0, True is 1
  m [] c cs = not c && null cs
  m (Zero : _) _ _ = False
  m (One : rs) c cs = not c && m rs c cs
  
  m (Let _ : _) _ [] = False
  m (Let a : rs) _ (x:xs) = a == x && m rs False xs
  m (Union r1 r2 : rs) c cs = m (r1:rs) c cs || m (r2:rs) c cs
  m (Cat r1 r2 : rs) c cs = m (r1:r2:rs) c cs || c && byp r1 && m (r2:rs) c cs
  m (Star r1 : rs) c cs = not c && m rs c cs || m (r1:Star r1:rs) True cs

-- | Matching algorithm 3: Circular matching programs
match3 :: RegExp -> String -> Bool
match3 r w = runmp (compile r) w

-- Matching programs
data MProg = Epsilon          -- Match the empty string
           | Fail             -- Match failure
           | Read Char MProg  -- Read a specific char then continue matching
           | Try MProg MProg  -- Try two matches in order (short-circuiting)

-- Run a matching program on a string
runmp :: MProg -> String -> Bool
runmp Epsilon w = null w
runmp Fail _ = False
runmp (Read _ _) [] = False
runmp (Read a mp) (x:xs) = a == x && runmp mp xs
runmp (Try mp1 mp2) w = runmp mp1 w || runmp mp2 w

-- | Compile a regular expression to a matching program with loops.
compile :: RegExp -> MProg
compile r = go r Epsilon where
  -- go r mp is the program that matches r and then runs mp on what's left
  -- uses nep in Star case to avoid infinite loops
  go :: RegExp -> MProg -> MProg
  go Zero _ = Fail
  go One mp = mp
  go (Let a) mp = Read a mp
  go (Union r1 r2) mp = Try (go r1 mp) (go r2 mp)
  go (Cat r1 r2) mp = let mp2  = go r2 mp
                          mp12 = go r1 mp2
                      in if byp r1 then Try mp12 mp2 else mp12
  go (Star Zero) mp = mp   -- Star Zero is One, which does nothing in compiled output
  go (Star One) mp = mp    -- Star One is One
  go (Star r1) mp = let loop = Try mp (go (nep r1) loop)
                    in loop
