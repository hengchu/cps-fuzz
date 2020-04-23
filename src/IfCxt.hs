module IfCxt where

import Control.Monad
import Data.Kind hiding (Type)
import Data.Proxy
import Language.Haskell.TH.Syntax

-- See https://github.com/mikeizbicki/ifcxt/blob/master/src/IfCxt.hs
-- for how this magic works.

class IfCxt (cxt :: Constraint) where
  ifCxt :: proxy cxt -> (cxt => a) -> a -> a

instance {-# OVERLAPPABLE #-} IfCxt cxt where
  ifCxt _ _ f = f

-- | Derives all possible instances of "IfCxt" for the given one parameter type class.
mkIfCxtInstances :: Name -> Q [Dec]
mkIfCxtInstances n = do
  info <- reify ''IfCxt
  let instancesOfIfCxt = case info of
        ClassI _ xs -> map (\(InstanceD _ _ (AppT _ t) _) -> t) xs
        _ -> error "mkIfCxtInstances: expected typeclass name"
      isInstanceOfIfCxt t = t `elem` instancesOfIfCxt
  info <- reify n
  case info of
    ClassI _ xs -> fmap concat $ forM xs $ \(InstanceD _ cxt (AppT classt t) _ys) ->
      return $
        if isInstanceOfIfCxt (AppT classt t)
          then []
          else mkInstance cxt classt t n
    _otherwise -> fail $ show n ++ " is not a class name."

mkInstance :: Cxt -> Type -> Type -> Name -> [Dec]
mkInstance cxt _classt t n =
  [ InstanceD
      Nothing
      (map relaxCxt cxt)
      (relaxCxt (AppT (ConT n) t))
      [ FunD
          'ifCxt
          [ Clause
              [ VarP $ mkName "_proxy",
                VarP $ mkName "t",
                VarP $ mkName "_f"
              ]
              (NormalB (mkIfCxtFun cxt))
              []
          ]
      ]
  ]

-- | "Relax" constraints by wrapping in "IfCxt".
relaxCxt :: Type -> Type
relaxCxt t@(AppT (ConT c) _) | c == ''IfCxt = t
relaxCxt t = AppT (ConT ''IfCxt) t

-- | Creates an implementation of "ifCxt". If our instance has no extra
-- constraints, e.g. deriving "IfCxt (Show Bool)" from "Show Bool", we simply
-- return the first argument.
--
-- If we have extra constraints, e.g. deriving
-- "IfCxt (Show a) => IfCxt(Show [a])" from "Show a => Show [a]", we call
-- "ifCxt" recursively to bring those instances in scope. We only return the
-- first argument if all constraints are satisfied.
mkIfCxtFun :: Cxt -> Exp
mkIfCxtFun [] = VarE $ mkName "t"
mkIfCxtFun (c : cs) =
  AppE
    ( AppE
        ( AppE
            (VarE 'ifCxt)
            proxy
        )
        (mkIfCxtFun cs)
    )
    (VarE $ mkName "_f")
  where
    proxy =
      SigE
        (ConE 'Proxy)
        (AppT (ConT ''Proxy) c)
