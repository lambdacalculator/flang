{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FLang.FSM 
  ( buildTable -- Export helper for completeness/debugging
  , formatTable
  , FSM
  , reachable
  , display
  , toDot
  , star
  , accept
  , intify
  , NFSM
  , nreachable
  , displayN
  , toDotN
  , hat
  , nacc

  , EFSM
  , ereachable
  ) where


import FLang.Utils (uclosure, norm, overlap)
import Data.List (intercalate, transpose, foldl')
import Data.Array (listArray, (!))


---------------- Finite State Machines ----------------------------------------------------

-- | deterministic Finite State Machine.
-- Defined implicitly by a start state, a transition function, and a final state predicate.
-- The set of states is implicit (those reachable from the start state).
type FSM a = (a, a -> Char -> a, a -> Bool)

-- reachable m == the list of states reachable from the start state
reachable :: (Ord a) => [Char] -> FSM a -> [a]
reachable sigma (s, d, _) = uclosure [s] (\q -> map (d q) sigma)

-- Generalized * construction for transition functions
star :: (a -> Char -> a) -> (a -> String -> a)
star = foldl'

-- Acceptance
accept :: FSM a -> String -> Bool
accept (s, d, fs) w = fs (star d s w)

-- Intify: Change the states of an FSM from an equality type to Int,
-- and use an array lookup for the transition function
intify :: forall a. (Ord a) => [Char] -> FSM a -> FSM Int
intify sigma m@(s, d, fs) = (s', d', fs') where
  qs = reachable sigma m
  n = length qs
  m_sig = length sigma
  s'  = ind qs s
  fs' q = fs (qs !! q)
  arr = listArray ((0,0), (n-1,m_sig-1)) [ind qs (d q a) | q <- qs, a <- sigma]
  d' q a = arr ! (q, ind sigma a)
  ind (q':rs) q = if q == q' then 0 else 1 + ind rs q
  ind [] _ = error "intify: state not found"


-- | Nondeterministic Finite State Machine.
-- Defined by a list of start states, a non-deterministic transition function, and a final state predicate.
type NFSM a = ([a], a -> Char -> [a], a -> Bool)

-- reachable states of NFSM
nreachable :: (Ord a) => [Char] -> NFSM a -> [a]
nreachable sigma (ss, d, _) = uclosure ss (\q -> concat $ map (d q) sigma)

-- | Hat construction: lifts a transition function to operate on sets (lists) of states.
-- Computes the set of states reachable from a set of states 'xs' on input 'a'.
hat :: Ord a => (a -> Char -> [a]) -> ([a] -> Char -> [a])
hat d xs a = FLang.Utils.norm $ concat [d q a | q <- xs]

-- | Check if an NFSM accepts a string.
nacc :: (Ord a) => NFSM a -> String -> Bool
nacc (ss, d, fs) w = any fs (star (hat d) ss w)


-- | Nondeterministic FSMs with epsilon moves.
-- M = (starts, transitions, epsilon-moves, isFinal)
-- d is a -> Char -> [a] (no epsilon here)
-- es is [(a,a)] (explicit epsilon)
type EFSM a = ([a], a -> Char -> [a], [(a,a)], a -> Bool)

ereachable :: (Ord a) => [Char] -> EFSM a -> [a]
ereachable sigma (ss, d, es, _) = uclosure ss (\q -> (concat $ map (d q) sigma) ++ [q2 | (q1,q2) <- es, q1==q])


---------------- Display and Visualization -----------------------------------------

display :: (Ord a, Show a) => [Char] -> FSM a -> String
display sigma m@(s, d, fs) = unlines $
  [ "States: " ++ show states
  , "Start: " ++ show s
  , "Finals: " ++ show finals
  , "Transition Table:"
  ] ++ formatTable (buildTable sigma states d)
  where
    states = reachable sigma m
    finals = filter fs states
  
toDot :: (Ord a, Show a) => [Char] -> FSM a -> String
toDot sigma m@(s, d, fs) = unlines $
  [ "digraph FSM {"
  , "  rankdir=LR;"
  , "  node [shape = doublecircle]; " ++ unwords (map show finals) ++ ";"
  , "  node [shape = circle];"
  , "  secret_start [style=invis, shape=point];"
  , "  secret_start -> " ++ show s ++ ";"
  ] ++ transitions ++ ["}"]
  where
    states = reachable sigma m
    finals = filter fs states
    transitions = [ "  " ++ show q1 ++ " -> " ++ show (d q1 a) ++ " [label=\"" ++ show a ++ "\"];" 
                  | q1 <- states, a <- sigma ]

displayN :: (Ord a, Show a) => [Char] -> NFSM a -> String
displayN sigma m@(ss, d, fs) = unlines $
  [ "States: " ++ show states
  , "Starts: " ++ show ss
  , "Finals: " ++ show finals
  , "Transition Table:"
  ] ++ formatTable (buildTable sigma states d)
  where
    states = nreachable sigma m
    finals = filter fs states

toDotN :: (Ord a, Show a) => [Char] -> NFSM a -> String
toDotN sigma m@(ss, d, fs) = unlines $
  [ "digraph NFSM {"
  , "  rankdir=LR;"
  , "  node [shape = doublecircle]; " ++ unwords (map show finals) ++ ";"
  , "  node [shape = circle];"
  ] ++ startArrows ++ transitions ++ ["}"]
  where
    states = nreachable sigma m
    finals = filter fs states
    startArrows = [ "  secret_start_" ++ show q ++ " [style=invis, shape=point]; secret_start_" ++ show q ++ " -> " ++ show q ++ ";" | q <- ss ]
    transitions = [ "  " ++ show q1 ++ " -> " ++ show q2 ++ " [label=\"" ++ show a ++ "\"];" 
                  | q1 <- states, a <- sigma, q2 <- d q1 a ]

-- Transition table helper
buildTable :: (Show a, Show r) => [Char] -> [a] -> (a -> Char -> r) -> [[String]]
buildTable sigma states delta = header : map mkRow sigma where
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
    -- Width = sum of column widths + (length colWidths - 1) * 2
    totalWidth = sum colWidths + (length colWidths - 1) * 2
    separator = replicate totalWidth '-'
  in
    formatRow (head t) : separator : map formatRow (tail t)