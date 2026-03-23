{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, DefaultSignatures, TypeOperators, FlexibleInstances #-}
-- |
-- Module      : FLang.CyclicShow
-- Description : Generic printing of cyclic data structures as let-bindings
-- Copyright   : (c) Todd Wilson, 2026
-- License     : BSD-3-Clause
--
-- This module provides a generic 'cyclicShow' function for printing
-- structurally cyclic data types (where sharing is tied via physical
-- memory knots) as readable 'let ... in ...' Haskell expressions.
--
-- To use this for your own data type @T@:
-- 1. Derive an isolated Functor version of your type @TF a@
-- 2. Provide an instance of 'MuRef' from "Data.Reify"
-- 3. Provide an instance of the 'CyclicPrint' class defined here to format the nodes.
-- 4. Define your 'Show' instance using 'cyclicShow'.

module FLang.CyclicShow
  ( cyclicShow
  , CyclicPrint(..)
  ) where

import Data.Reify        (MuRef(..), Graph(..), reifyGraph)
import System.IO.Unsafe  (unsafePerformIO)
import Data.List         (intercalate, sortBy)
import Data.Ord          (comparing)
import Data.Foldable     (toList)
import GHC.Generics

-- | A class that tells the cyclic printer how to format the contents
-- of a single node in your graph.
class CyclicPrint f where
  -- | Format a node given a formatting map for its recursive children.
  -- This should return the "inline" string representation.
  -- The engine handles inserting parentheses if the child strings
  -- contain spaces.
  formatNode :: (e -> String) -> f e -> String
  default formatNode :: (Generic1 f, GCyclicPrint1 (Rep1 f)) => (e -> String) -> f e -> String
  formatNode f x = gFormatNode1 f (from1 x)
  
  -- | Identify if a node is "trivial" (like a leaf or empty constructor)
  -- so that the engine doesn't hoist it into a 'let' binding even if shared.
  isTrivialNode :: f e -> Bool
  default isTrivialNode :: (Generic1 f, GCyclicPrint1 (Rep1 f)) => f e -> Bool
  isTrivialNode x = gIsTrivialNode1 (from1 x)

-- | Generic1 traversal class for deriving CyclicPrint
class GCyclicPrint1 rep where
  gFormatNode1 :: (e -> String) -> rep e -> String
  gIsTrivialNode1 :: rep e -> Bool

-- | Datatype wrapper (D1)
instance GCyclicPrint1 f => GCyclicPrint1 (D1 c f) where
  gFormatNode1 f (M1 x) = gFormatNode1 f x
  gIsTrivialNode1 (M1 x) = gIsTrivialNode1 x

-- | Constructor wrapper (C1)
instance (Constructor c, GCyclicPrint1 f) => GCyclicPrint1 (C1 c f) where
  gFormatNode1 f con@(M1 x) = 
    let name = conName con
        cleanName = if not (null name) && last name == 'F' then init name else name
        args = gFormatNode1 f x
    in if null args then cleanName else cleanName ++ " " ++ args
  gIsTrivialNode1 _ = False -- Constructors with args are not trivial

-- | Constructors with NO arguments (U1) are trivial
instance GCyclicPrint1 U1 where
  gFormatNode1 _ U1 = ""
  gIsTrivialNode1 U1 = True

-- | Sums (:+:)
instance (GCyclicPrint1 f, GCyclicPrint1 g) => GCyclicPrint1 (f :+: g) where
  gFormatNode1 f (L1 x) = gFormatNode1 f x
  gFormatNode1 f (R1 x) = gFormatNode1 f x
  gIsTrivialNode1 (L1 x) = gIsTrivialNode1 x
  gIsTrivialNode1 (R1 x) = gIsTrivialNode1 x

-- | Products (:*:)
instance (GCyclicPrint1 f, GCyclicPrint1 g) => GCyclicPrint1 (f :*: g) where
  gFormatNode1 f (x :*: y) = gFormatNode1 f x ++ " " ++ gFormatNode1 f y
  gIsTrivialNode1 _ = False

-- | Field wrappers (S1)
instance GCyclicPrint1 f => GCyclicPrint1 (S1 c f) where
  gFormatNode1 f (M1 x) = gFormatNode1 f x
  gIsTrivialNode1 (M1 x) = gIsTrivialNode1 x

-- | The critical part: intercepting the edge parameter 'e' (Par1)
instance GCyclicPrint1 Par1 where
  gFormatNode1 f (Par1 x) = f x 
  gIsTrivialNode1 _ = False

-- | Primitive payloads (K1) that do not contain the edge parameter!
-- We assume these have a standard 'Show' instance.
instance Show a => GCyclicPrint1 (K1 i a) where
  gFormatNode1 _ (K1 x) = show x
  gIsTrivialNode1 _ = False

-- | Render any 'MuRef' instance whose functor implements 'CyclicPrint'
-- and 'Foldable' into a 'let ... in ...' structurally shared String.
cyclicShow :: forall a f. (MuRef a, DeRef a ~ f, CyclicPrint f, Foldable f) => a -> String
cyclicShow root = unsafePerformIO $ do
  Graph env rootId <- reifyGraph root
  
  let isTriv :: Int -> Bool
      isTriv u = case lookup u env of
                   Just node -> isTrivialNode node
                   Nothing -> False
                   
      edges :: [Int]
      edges = rootId : concatMap (\(_, node) -> toList node) env
            
      inDegree :: Int -> Int
      inDegree u = length (filter (== u) edges)
      
      namedNodes = [ u | (u, _) <- env, not (isTriv u), inDegree u > 1 ]
      
      rootNamed = namedNodes
                  
      sortedNamed = zip (sortBy (comparing id) rootNamed) ["p" ++ show i | i <- [(1::Int)..]]
      
      getName :: Int -> Maybe String
      getName u = lookup u sortedNamed
      
      formatChild :: Int -> String
      formatChild u = case getName u of
          Just name -> name
          Nothing -> 
             let s = formatU u
             in if ' ' `elem` s then "(" ++ s ++ ")" else s
             
      formatU :: Int -> String
      formatU u = case lookup u env of
          Just node -> formatNode formatChild node
          Nothing -> "???"
          
  if null sortedNamed
    then return $ formatU rootId
    else do
      let bnds = [ name ++ " = " ++ formatU u | (u, name) <- sortedNamed ]
          letBlock = "let " ++ intercalate "\n    " bnds ++ "\nin " ++ case getName rootId of Just n -> n; Nothing -> formatU rootId
      return letBlock
