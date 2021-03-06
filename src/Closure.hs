{-|
Module: Closure
Description: Implementations of top-level closure conversion
-}
module Closure where

import Control.Monad.Catch
import Data.Foldable
import qualified Data.Set as S
import HFunctor
import Names
import Syntax
import Text.Printf
import Type.Reflection

-- |The representation of a type-indexed top-level closure. Basically a
-- type-indexed n-tuple of free variable names in a lambda.
data Closure :: * -> * where
  C1 :: Typeable a => Var a -> Closure a
  CCons :: (Typeable a, Typeable b) => Closure a -> Var b -> Closure (a, b)

-- |Convert the extracted closure into a term.
closureTerm :: (HInject ExprF h, HInject PrimF h) => Closure cls -> HFix h cls
closureTerm (C1 v) = wrap . hinject' $ EVarF v
closureTerm (CCons (closureTerm -> cls') v) = pair cls' (wrap . hinject' $ EVarF v)

-- |Feed the extracted closure to a continuation.
buildClosure :: [AnyVar] -> (forall cls. Typeable cls => Closure cls -> r) -> r
buildClosure [] _ = error "buildClosure: expected at least 1 free variable"
buildClosure [AnyVar x] k = k (C1 x)
buildClosure ((AnyVar x) : xs) k = buildClosure xs $ \cls -> k $ CCons cls x

-- |Errors that may arise during closure conversion.
data ClosureConvertError =
  -- |InternalError is a compiler bug.
  InternalError String
  deriving (Show)

instance Exception ClosureConvertError

-- |Checks whether a variable exists in some closure.
contains :: Typeable a => Closure cls -> Var a -> Bool
contains (C1 x) y = AnyVar x == AnyVar y
contains (CCons left right) y = AnyVar right == AnyVar y || contains left y

-- |Build a projection function from the closure for a given variable.
prj ::
  forall cls a h m.
  ( MonadThrowWithStack m,
    Typeable a,
    HInject PrimF h
  ) =>
  Closure cls ->
  Var a ->
  m (HFix h cls -> HFix h a)
prj (C1 y) x =
  case eqTypeRep (typeRep @cls) (typeRep @a) of
    Just HRefl ->
      if y == x
        then return id
        else throwM' . InternalError $ printf "prj: cannot find variable %s" (show x)
    _ -> throwM' . InternalError $ printf "prj: cannot find variable %s" (show x)
prj (CCons left (right :: _ b)) x
  | left `contains` x = do
    f <- prj left x
    return $ f . pfst
  | otherwise =
    case eqTypeRep (typeRep @b) (typeRep @a) of
      Just HRefl ->
        if right == x
          then return psnd
          else throwM' . InternalError $ printf "prj: cannot find variable %s" (show x)
      _ -> throwM' . InternalError $ printf "prj: cannot find variable %s" (show x)

-- |The result of closure conversion.
data Result :: * -> * where
  -- |The input function contains no top-level free variable.
  AlreadyClosed :: HFix h (() -> a -> b) -> Result (HFix h (a -> b))
  -- |The input function has been closure converted into a closed function, and
  -- the extracted closure.
  Converted :: Typeable cls => HFix h cls -> HFix h (cls -> a -> b) -> Result (HFix h (a -> b))

-- | Closure convert the top-level lambda. Taking out its free variables, and
-- constructs a new term from those free variables that represents the closure,
-- and a new lambda that takes the closure as an argument, and is a closed
-- program.
closureConvert ::
  forall h a b m.
  ( HInject ExprF h,
    HInject PrimF h,
    Typeable a,
    Typeable b,
    HFunctor h,
    MonadThrowWithStack m,
    FreshM m
  ) =>
  HFix h (a -> b) ->
  (forall a. HFix h a -> S.Set AnyVar) ->
  m (Result (HFix h (a -> b)))
closureConvert lambda fvFun = do
  let fvs = fvFun lambda
  case S.null fvs of
    True -> do
      emptyClsName <- gfresh "empty_closure"
      return $ AlreadyClosed . wrap . hinject' $ (ELamF (Var emptyClsName) lambda)
    False -> buildClosure (S.toList fvs) $ \(cls :: _ cls) -> do
      let clsTerm = closureTerm @h cls
      clsName <- gfresh "closure"
      let clsVar = Var @cls clsName
      let clsVarTerm :: HFix h cls
          clsVarTerm = wrap . hinject' $ EVarF clsVar
      lambda' <-
        foldrM
          ( \(AnyVar fv) lam -> do
              prjfun <- prj cls fv
              return $ substGen fv (prjfun clsVarTerm) lam
          )
          lambda
          fvs
      let clsLambda = wrap . hinject' $ ELamF clsVar lambda'
      return $ Converted clsTerm clsLambda
