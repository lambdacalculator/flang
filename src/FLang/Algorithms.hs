module FLang.Algorithms 
  ( thompson
  , glushkov
  , brzozowski
  , elim_eps
  , nfsm_to_fsm
  , reverseFSM
  , opFSM
  , nreachable
  , minimize
  , fsm_to_re
  , solve
  ) where

import FLang.Language (merge)
import FLang.FSM
import FLang.RegExp (RegExp(..), (<+>), (<.>), mkStar)
import qualified FLang.RegExp as RE
import FLang.Utils (norm, cart, diff, eq2part, uclosure)



---- Conversions from RegExp to FSM, NFSM, and EFSM ---------------------------

-- | Thompson's construction: converts a RegExp to an EFSM.
-- The states are integers. A fresh state count is maintained during construction.
thompson :: RegExp -> EFSM Int
thompson r = (ss, d, es, fs) where
  (_, ss, fs_idx, d, es) = build r 0

  fs q = q `elem` fs_idx
  
  -- Internal build function returning (next_state_index, start_states, final_states, transition_fn, epsilon_moves)
  build :: RegExp -> Int -> (Int, [Int], [Int], Int -> Char -> [Int], [(Int,Int)])
  build Zero k = (k, [], [], \_ _ -> [], [])
  build One k = (k+1, [k], [k], \_ _ -> [], []) -- One accepts empty string
  build (Let c) k = (k+2, [k], [k+1], d', []) where
    d' q a = if q == k && a == c then [k+1] else []
  build (Union r1 r2) k = (n2, ss1 ++ ss2, fs1 ++ fs2, d', es1 ++ es2) where
    (n1, ss1, fs1, d1, es1) = build r1 k
    (n2, ss2, fs2, d2, es2) = build r2 n1
    d' q a = if q < n1 then d1 q a else d2 q a 
  build (Cat r1 r2) k = (n2, ss1, fs2, d', es_new) where
    (n1, ss1, fs1, d1, es1) = build r1 k
    (n2, ss2, fs2, d2, es2) = build r2 n1
    d' q a = if q < n1 then d1 q a else d2 q a 
    es_new = es1 ++ cart fs1 ss2 ++ es2 
  build (Star r1) k = (n1, [k], [k], d', es_new) where
    (n1, ss1, fs1, d1, es1) = build r1 (k+1)
    d' q a = if q == k then [] else d1 q a
    es_new = cart [k] ss1 ++ es1 ++ cart fs1 [k]

-- | Glushkov construction: converts a RegExp to an NFSM.
glushkov :: RegExp -> NFSM Int
glushkov r = (ss, d_final, fs) where
  n_total = RE.numLets r
  (_, xs, b, d_final) = rcat r n_total [n_total] (\_ _ -> [])
  
  ss = if b then xs ++ [n_total] else xs
  fs q = q == n_total

  -- Helper: rcat
  rcat :: RegExp -> Int -> [Int] -> (Int -> Char -> [Int]) -> (Int, [Int], Bool, (Int -> Char -> [Int]))
  rcat Zero j _ d' = (j, [], False, d')
  rcat One j _ d' = (j, [], True, d')
  rcat (Let c) j ss' d' = (i, [i], False, d'') where
    i = j - 1
    d'' q a = if q == i && a == c then ss' else d' q a
  rcat (Union r1 r2) j ss' d' = (n1, xs1 ++ xs2, b1 || b2, d'') where
    (n2, xs2, b2, d2) = rcat r2 j ss' d'
    (n1, xs1, b1, d1) = rcat r1 n2 ss' d2
    d'' q a = if q < n2 then d1 q a else d2 q a
  rcat (Cat r1 r2) j ss' d' = (n1, xs_final, b1 && b2, d'') where
    (n2, xs2, b2, d2) = rcat r2 j ss' d'
    ss2 = if b2 then merge xs2 ss' else xs2
    (n1, xs1, b1, d1) = rcat r1 n2 ss2 d2
    xs_final = if b1 then xs1 ++ xs2 else xs1
    d'' q a = if q < n2 then d1 q a else d2 q a
  rcat (Star r1) j ss' d' = (n1, xs1, True, d1) where
    (n1, xs1, _, d1) = rcat r1 j (merge xs1 ss') d'

-- | Brzozowski derivative construction: converts a RegExp to an FSM.
-- Uses the smart left quotient (<\>) to ensure the state space is finite.
brzozowski :: RegExp -> FSM RegExp
brzozowski r = (s, d, fs) where
  s = r
  fs = RE.byp
  d = flip (RE.<\>)


---- Algorithms ---------------------------

-- Eliminate epsilon moves
elim_eps :: (Ord a) => EFSM a -> NFSM a
elim_eps (ss, d', es, fs) = (ss', de, fs) where
  close q = uclosure [q] (\x -> [q2 | (q1,q2) <- es, q1 == x])
  ss' = norm $ concatMap close ss
  de q a = norm $ concatMap close (d' q a)

-- Conversion from NFSM to FSM ("subset construction")
nfsm_to_fsm :: (Ord a) => NFSM a -> FSM [a]
nfsm_to_fsm (ss, d', fs) = (ss, hat d', fs') where
  fs' qs = any fs qs

-- Reverse FSM to a NFSM
reverseFSM :: (Ord a) => [Char] -> FSM a -> NFSM a
reverseFSM sigma m@(s, d', fs) = (finals, d_rev, fs') where
  states = reachable sigma m
  finals = filter fs states
  d_rev q a = [q' | q' <- states, d' q' a == q]
  fs' q = q == s

-- | Machine that accepts the 'op' of the languages accepted by m1 and m2.
opFSM :: (Ord a, Ord b) => (Bool -> Bool -> Bool) -> FSM a -> FSM b -> FSM (a, b)
opFSM op (s1, d1, fs1) (s2, d2, fs2) = ((s1, s2), d', fs) where
  d' (q1, q2) a = (d1 q1 a, d2 q2 a)
  fs (q1, q2) = fs1 q1 `op` fs2 q2

-- | Minimization of a deterministic FSM. Requires sigma.
-- Implemented via backward reachability on the product machine (finding distinguishable pairs).
-- This is functionally equivalent to Hopcroft's n log n algorithm but O(n^2) due to fixpoint computation.
minimize :: (Ord a) => [Char] -> FSM a -> FSM [a]
minimize sigma m@(s1, d1, fs1) = (s, d', fs) where
  qs = reachable sigma m
  
  -- 1. Identify distinguishable pairs (differing in finality)
  d0 = [(p,q) | p <- qs, q <- qs, fs1 p /= fs1 q]
  
  -- Reverse transition map for M
  (_, d_rev_m, _) = reverseFSM sigma m 
  
  -- Reverse transition for pairs (conceptually reversing the product machine)
  -- (p',q') -> (p,q) if p' -> p and q' -> q
  d_rev_pair (p,q) a = [ (p',q') | p' <- d_rev_m p a, q' <- d_rev_m q a ]
  
  -- Compute closure of distinguishable pairs (backward reachability)
  distinguishable = uclosure d0 (\pq -> concat [d_rev_pair pq a | a <- sigma])
  
  allPairs = cart qs qs
  equivalent = diff allPairs distinguishable
  
  -- Partitions
  partitions = eq2part qs equivalent
  
  -- New states are partitions
  s = getBlock s1 partitions
  d' block a = getBlock (d1 (head block) a) partitions
  fs block = fs1 (head block)
  
  getBlock _ [] = error "State not found in partitions"
  getBlock q (b:bs) = if q `elem` b then b else getBlock q bs


---- Conversion from FSM to RegExp -------------------------------------------

-- | Solves a system of linear regular expression equations of the form:
-- X_i = (A_i1 . X_1) + ... + (A_in . X_n) + B_i
-- Uses Gaussian elimination
solve :: [[RegExp]] -> [RegExp] -> [RegExp]
solve [] [] = []
solve ((a11:a1J) : rows) (b1:bs) = x1 : xRest
  where
    -- Pivot: A_11*
    s11 = mkStar a11

    -- Update remaining matrix: A'_ij = A_ij + (A_i1 . s11 . A_1j)
    rows' = zipWith updateRow rows (map head rows)
      where
        -- row is A_i2...A_in, ai1 is the coefficient for the eliminated variable
        updateRow row ai1 = zipWith (\aij a1j -> aij <+> (ai1 <.> s11 <.> a1j)) (tail row) a1J

    -- Update remaining constants: B'_i = B_i + (A_i1 . s11 . B_1)
    bs' = zipWith (\bi ai1 -> bi <+> (ai1 <.> s11 <.> b1)) bs (map head rows)

    -- Recursively solve the smaller system
    xRest = solve rows' bs'

    -- Back Substitution for x1: X_1 = s11 . (B_1 + sum(A_1j . X_j))
    -- Note: We use foldr to sum the dot product.
    sum_a1j_xj = foldr (<+>) Zero (zipWith (<.>) a1J xRest)
    x1 = s11 <.> (b1 <+> sum_a1j_xj)
solve _ _ = error "solve: mismatched system"

-- Helper for finding index of an element (assumes existence)
index :: Eq a => a -> [a] -> Int
index x (y:ys) 
  | x == y    = 0
  | otherwise = 1 + index x ys
index _ [] = error "State not found in reachable set"

-- | Convert FSM to RegExp. Requires sigma.
fsm_to_re :: (Ord a) => [Char] -> FSM a -> RegExp
fsm_to_re sigma m@(s, d, fs) = solution !! index s qs
  where
    qs = reachable sigma m
    
    -- Build the coefficient matrix A
    -- A_ij = Sum of chars c such that d(q_i, c) = q_j
    coef q1 q2 = foldr (<+>) Zero [RE.Let a | a <- sigma, d q1 a == q2]
    
    -- Build the constant vector B
    -- B_i = One if q_i is final, else Zero
    final q = if fs q then RE.One else RE.Zero
    
    solution = solve [[coef q1 q2 | q2 <- qs] | q1 <- qs] [final q | q <- qs]
