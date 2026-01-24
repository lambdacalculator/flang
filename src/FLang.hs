module FLang
  ( -- * Regular Expressions
    RegExp(..)
  , (<+>), (<.>)
  , toRE
  , match3
    -- * Finite State Machines
  , FSM, NFSM, EFSM
  , Machine(..), validate
  , reachable, minimize, intify, nreachable

  , toDot
    -- * Algorithms
  , brzozowski
  ) where


import FLang.RegExp
import FLang.FSM
import FLang.Algorithms
