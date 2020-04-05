module IState
  ( IState(..)
  , IStateT
  , initIState
  ) where

import Control.Monad.Trans.State.Lazy

import Syntax
import Type

data IState = IState
  { freshId :: Int
  , env :: Env
  , tyenv :: TyEnv
  }

type IStateT = StateT IState

initIState :: IState
initIState = IState
  { freshId = 0
  , env = []
  , tyenv = []
  }
