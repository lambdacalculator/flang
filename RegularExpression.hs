module RegularExpression where

import Data.List (foldl', nub)

-- | Standard Regular Expressions.
-- We use a binary representation to enforce right-associativity via constructors.
data RegExp
  = Zero
  | One
  | Sym Char
  | Union RegExp RegExp  -- Invariant: Right-associative, deep idempotent
  | Cat RegExp RegExp    -- Invariant: Right-associative, Zero/One simplified
  | Star RegExp          -- Invariant: Star Zero/One/Star simplified
  deriving (Eq, Ord, Show)

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
mkUnion p q =
  case p of
    -- 1. Maintain Right-Associativity (Rotate)
    Union a b -> mkUnion a (mkUnion b q)
    -- 2. Atomic p: Insert into q (Deep Dedupe)
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
-- 1. Associativity: (AB)C -> ABC
-- 2. Identity: One.x -> x, x.One -> x
-- 3. Annihilation: Zero.x -> Zero, x.Zero -> Zero
-- CRITICAL: Does NOT distribute. Preserves factored forms.
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
-- Brzozowski Derivatives
--------------------------------------------------------------------------------

-- | Nullable predicate (matches empty string?)
nullable :: RegExp -> Bool
nullable Zero        = False
nullable One         = True
nullable (Sym _)     = False
nullable (Union p q) = nullable p || nullable q
nullable (Cat p q)   = nullable p && nullable q
nullable (Star _)    = True

-- | The Brzozowski Derivative (Left Quotient).
-- Uses smart constructors to ensure the set of derivatives is finite.
lq :: Char -> RegExp -> RegExp
lq _ Zero        = Zero
lq _ One         = Zero
lq c (Sym x)     = if c == x then One else Zero
lq c (Union p q) = lq c p <+> lq c q
lq c (Cat p q)   = (lq c p <.> q) <+> (if nullable p then lq c q else Zero)
lq c (Star r)    = lq c r <.> mkStar r

-- | State transition function (Derivative + Normalization).
-- Since constructors normalize, this is just 'lq'.
step :: Char -> RegExp -> RegExp
step = lq

--------------------------------------------------------------------------------
-- FSM to RegExp (Gaussian Elimination / Arden's Lemma)
--------------------------------------------------------------------------------

type FSM a = (a, a -> Char -> a, a -> Bool)

-- | Solves a system of linear regular expression equations of the form:
-- X_i = (A_i1 . X_1) + ... + (A_in . X_n) + B_i
-- Uses the "Sixes and Sevens" elimination method (Gaussian elimination).
solve :: [[RegExp]] -> [RegExp] -> [RegExp]
solve [] [] = []
solve ((a11:a1J) : rows) (b1:bs) = x1 : xRest
  where
    -- Pivot: A_11*
    s11 = mkStar a11

    -- Update remaining matrix (The "Sixes"): A'_ij = A_ij + (A_i1 . s11 . A_1j)
    rows' = zipWith updateRow rows (map head rows)
      where
        -- row is A_i2...A_in, ai1 is the coefficient for the eliminated variable
        updateRow row ai1 = zipWith (\aij a1j -> aij <+> (ai1 <.> s11 <.> a1j)) row a1J

    -- Update remaining constants (The "Sevens"): B'_i = B_i + (A_i1 . s11 . B_1)
    bs' = zipWith (\bi ai1 -> bi <+> (ai1 <.> s11 <.> b1)) bs (map head rows)

    -- Recursively solve the smaller system
    xRest = solve rows' bs'

    -- Back Substitution for x1: X_1 = s11 . (B_1 + sum(A_1j . X_j))
    -- Note: We use foldr to sum the dot product.
    sum_a1j_xj = foldr (<+>) Zero (zipWith (<.>) a1J xRest)
    x1 = s11 <.> (b1 <+> sum_a1j_xj)

-- | Converts an FSM to a Regular Expression for the start state.
-- Assumes the alphabet 'sigma' is provided or derivable (here hardcoded for example).
fsm_to_re :: (Ord a, Eq a) => [Char] -> FSM a -> RegExp
fsm_to_re sigma m@(s, d, fs) = solution !! index s qs
  where
    qs = reachable m
    
    -- Build the coefficient matrix A
    -- A_ij = Sum of chars c such that d(q_i, c) = q_j
    coef q1 q2 = foldr (<+>) Zero [Sym a | a <- sigma, d q1 a == q2]
    
    -- Build the constant vector B
    -- B_i = One if q_i is final, else Zero
    final q = if fs q then One else Zero
    
    solution = solve [[coef q1 q2 | q2 <- qs] | q1 <- qs] [final q | q <- qs]

    index x (y:ys) 
      | x == y    = 0
      | otherwise = 1 + index x ys
    index _ [] = error "State not found in reachable set"

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

-- | Standard BFS for reachable states.
reachable :: (Ord a, Eq a) => FSM a -> [a]
reachable (s, d, _) = bfs [s] [s]
  where
    -- ASCII range for sigma, or pass it in. Assuming standard printable.
    sigma = [' ' .. '~'] 
    
    bfs [] _ = []
    bfs (q:qs) visited = 
      let 
        nextStates = [d q c | c <- sigma]
        newStates = filter (`notElem` visited) nextStates
      in 
        q : bfs (qs ++ newStates) (visited ++ newStates)
