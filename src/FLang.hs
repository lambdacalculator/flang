module FLang
  ( -- * Regular Expressions
    RegExp(..)
  , HasSigma(..)
  , (<+>), (<.>), star
  , toRE
  , match3
    -- * Finite State Machines
  , FSM, NFSM, EFSM
  , reachable, minimize
  , toDot
    -- * Algorithms
  , brzozowski
  ) where

import FLang.Language (HasSigma(..))
import FLang.RegExp
import FLang.FSM hiding (star)
import FLang.Algorithms
