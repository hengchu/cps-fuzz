module Compiler where

import IfCxt
import Control.Lens
import Control.Monad.State.Strict
import qualified Data.Map.Strict as M
import Lib
import Names
import Type.Reflection

data AnyExprFun :: * where
  MkAnyExprFun :: (Typeable b) => Expr (a -> b) -> AnyExprFun

data CompilerState
  = CompilerState
      { _csNames :: NameState
      , _csReprSize :: Int
      , _csMapFunction :: AnyExprFun
      , _csReleaseFunction :: Expr (Vec Number -> Number)
      }

makeLensesWith abbreviatedFields ''CompilerState

emptyCompilerState :: CompilerState
emptyCompilerState = undefined

newtype Compiler a = Compiler {runCompiler_ :: State CompilerState a}
  deriving (Functor, Applicative, Monad, MonadState CompilerState) via (State CompilerState)

runCompiler :: Compiler a -> (a, CompilerState)
runCompiler = flip runState emptyCompilerState . runCompiler_

compile ::
  forall a m.
  ( CFT a,
    MonadState CompilerState m,
    FreshM m
  ) =>
  (CPSFuzz (Bag a) -> CPSFuzz Number) ->
  String ->
  m (BMCS Number)
compile prog dbName = undefined

compile' ::
  forall r m.
  ( CFT r,
    BT r,
    MonadState CompilerState m,
    FreshM m
  ) =>
  CPSFuzz r ->
  m ()
compile' = undefined

-- ##################
-- # INFRASTRUCTURE #
-- ##################

instance FreshM Compiler where
  getNameState = gets (^. names)
  modifyNameState f = modify (\st -> st & names %~ f)
