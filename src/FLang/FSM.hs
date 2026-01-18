{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FLang.FSM 
  ( Machine(..)
  , buildTable -- Export helper for completeness/debugging
  , formatTable
  , FSM
  , reachable
  , star
  , accept
  , intify
  , NFSM
  , nreachable
  , hat
  , nacc
  , nacc'
  , EFSM
  , ereachable
  ) where

import FLang.Language (HasSigma(sigma))
import FLang.Utils (uclosure, norm, overlap)
import Data.List (intercalate, transpose, foldl')
import Data.Array (listArray, (!))

---------------- Finite State Machines ----------------------------------------------------

-- | A typeclass for machines that can be displayed or exported to Graphviz.
class Machine m where
    display :: m -> String
    toDot :: m -> String

buildTable :: (HasSigma s, Show s, Show a, Show r) => [a] -> (a -> s -> r) -> [[String]]
buildTable states delta = header : map mkRow sigma where
  header = "δ" : map show states
  mkRow c = show c : map (\q -> show (delta q c)) states

-- Formatting Logic (calculates widths and aligns columns)
formatTable :: [[String]] -> [String]
formatTable t =
  let
    -- Calculate width of each column (max width of any cell in that column)
    colWidths :: [Int]
    colWidths = map (maximum . map length) (transpose t)

    -- Pad a string to the specified width (left-aligned)
    pad :: Int -> String -> String
    pad w s = s ++ replicate (w - length s) ' '

    -- Format a row with 2 spaces between columns
    formatRow :: [String] -> String
    formatRow r = intercalate "  " $ zipWith pad colWidths r

    -- Create the separator line matching the full table width
    -- Width = sum of column widths + 2 spaces per gap
    totalWidth = sum colWidths + (length colWidths - 1) * 2
    separator = replicate totalWidth '-'
  in
    formatRow (head t) : separator : map formatRow (tail t)

-- | deterministic Finite State Machine.
-- Defined implicitly by a start state, a transition function, and a final state predicate.
-- The set of states is implicit (those reachable from the start state).
type FSM s a = (a, a -> s -> a, a -> Bool)

-- reachable m == the list of states reachable from the start state
reachable :: (HasSigma s, Ord a) => FSM s a -> [a]
reachable (s, d, _) = uclosure [s] (\q -> map (d q) sigma)

instance (HasSigma s, Show s, Ord a, Show a) => Machine (FSM s a) where
  display m@(s, d, fs) = unlines $
    [ "States: " ++ show states
    , "Start: " ++ show s
    , "Finals: " ++ show finals
    , "Transition Table:"
    ] ++ formatTable (buildTable states d)
    where
      states = reachable m
      finals = filter fs states
  
  toDot m@(s, d, fs) = unlines $
    [ "digraph FSM {"
    , "  rankdir=LR;"
    , "  node [shape = doublecircle]; " ++ unwords (map show finals) ++ ";"
    , "  node [shape = circle];"
    , "  secret_start [style=invis, shape=point];"
    , "  secret_start -> " ++ show s ++ ";"
    ] ++ transitions ++ ["}"]
    where
      states = reachable m
      finals = filter fs states
      transitions = [ "  " ++ show q1 ++ " -> " ++ show (d q1 a) ++ " [label=\"" ++ show a ++ "\"];" 
                    | q1 <- states, a <- sigma ]

-- Generalized * construction for transition functions
star :: (a -> s -> a) -> (a -> [s] -> a)
star = foldl'

-- Acceptance
accept :: (Eq a) => FSM s a -> [s] -> Bool
accept (s, d, fs) w = fs (star d s w)


-- Intify: Change the states of an FSM from an equality type to Int,
-- and use an array lookup for the transition function
intify :: forall s a. (HasSigma s, Ord a) => FSM s a -> FSM s Int
intify m@(s, d, fs) = (s', d', fs') where
  qs = reachable m
  n = length qs
  m_sig = length (sigma :: [s])
  s'  = ind qs s
  fs' q = fs (qs !! q)
  arr = listArray ((0,0), (n-1,m_sig-1)) [ind qs (d q a) | q <- qs, a <- sigma]
  d' q a = arr ! (q, ind sigma a)
  ind (q':rs) q = if q == q' then 0 else 1 + ind rs q
  ind [] _ = error "intify: state not found"

-- | Nondeterministic Finite State Machine.
-- Defined by a list of start states, a non-deterministic transition function, and a final state predicate.
type NFSM s a = ([a], a -> s -> [a], a -> Bool)

-- reachable states of NFSM
nreachable :: (HasSigma s, Ord a) => NFSM s a -> [a]
nreachable (ss, d, _) = uclosure ss (\q -> concat $ map (d q) sigma)

instance (HasSigma s, Show s, Ord a, Show a) => Machine (NFSM s a) where
  display m@(ss, d, fs) = unlines $
    [ "States: " ++ show states
    , "Starts: " ++ show ss
    , "Finals: " ++ show finals
    , "Transition Table:"
    ] ++ formatTable (buildTable states d)
    where
      states = nreachable m
      finals = filter fs states

  toDot m@(ss, d, fs) = unlines $
    [ "digraph NFSM {"
    , "  rankdir=LR;"
    , "  node [shape = doublecircle]; " ++ unwords (map show finals) ++ ";"
    , "  node [shape = circle];"
    ] ++ startArrows ++ transitions ++ ["}"]
    where
      states = nreachable m
      finals = filter fs states
      startArrows = [ "  secret_start_" ++ show q ++ " [style=invis, shape=point]; secret_start_" ++ show q ++ " -> " ++ show q ++ ";" | q <- ss ]
      transitions = [ "  " ++ show q1 ++ " -> " ++ show q2 ++ " [label=\"" ++ show a ++ "\"];" 
                    | q1 <- states, a <- sigma, q2 <- d q1 a ]


-- | Hat construction: lifts a transition function to operate on sets (lists) of states.
-- Computes the set of states reachable from a set of states 'xs' on input 'a'.
hat :: Ord a => (a -> s -> [a]) -> ([a] -> s -> [a])
hat d xs a = FLang.Utils.norm $ concat [d q a | q <- xs]

-- | Check if an NFSM accepts a string (inefficient version using overlap).
-- Note: Requires HasSigma to compute the full reachable set for intersection.
nacc :: (HasSigma s, Ord a) => NFSM s a -> [s] -> Bool
nacc (ss, d, fs) w = FLang.Utils.overlap (star (hat d) ss w) (filter fs (nreachable (ss,d,fs)))

-- | Check if an NFSM accepts a string (efficient version).
nacc' :: (Ord a) => NFSM s a -> [s] -> Bool
nacc' (ss, d, fs) w = any fs (star (hat d) ss w)


-- Nondeterministic FSMs with epsilon moves
-- M = (starts, transitions, epsilon-moves, isFinal)
type EFSM s a = ([a], a -> s -> [a], [(a,a)], a -> Bool)

ereachable :: (HasSigma s, Ord a) => EFSM s a -> [a]
ereachable (ss, d, es, _) = uclosure ss (\q -> (concat $ map (d q) sigma) ++ [q2 | (q1,q2) <- es, q1==q])

instance (HasSigma s, Show s, Ord a, Show a) => Machine (EFSM s a) where
  display m@(ss, d, es, fs) = unlines $
    [ "States: " ++ show states
    , "Starts: " ++ show ss
    , "Finals: " ++ show finals
    , "Epsilon: " ++ show es
    , "Transition Table:"
    ] ++ formatTable (buildTable states d)
    where
      states = ereachable m
      finals = filter fs states

  toDot m@(ss, d, es, fs) = unlines $
    [ "digraph EFSM {"
    , "  rankdir=LR;"
    , "  node [shape = doublecircle]; " ++ unwords (map show finals) ++ ";"
    , "  node [shape = circle];"
    ] ++ startArrows ++ transitions ++ epsTransitions ++ ["}"]
    where
      states = ereachable m
      finals = filter fs states
      startArrows = [ "  secret_start_" ++ show q ++ " [style=invis, shape=point]; secret_start_" ++ show q ++ " -> " ++ show q ++ ";" | q <- ss ]
      transitions = [ "  " ++ show q1 ++ " -> " ++ show q2 ++ " [label=\"" ++ show a ++ "\"];" 
                    | q1 <- states, a <- sigma, q2 <- d q1 a ]
      epsTransitions = [ "  " ++ show q1 ++ " -> " ++ show q2 ++ " [label=\"ε\"];" 
                       | (q1, q2) <- es, q1 `elem` states ]

