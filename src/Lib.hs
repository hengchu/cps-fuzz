{-# LANGUAGE AllowAmbiguousTypes #-}

module Lib where

import Control.Monad.State.Strict
import Data.Constraint
import Data.Functor.Identity
import qualified Data.Set as S
import Names
import Type.Reflection

newtype Bag a = Bag [a]
  deriving (Show, Eq, Ord, Foldable, Functor, Traversable)

newtype Vec a = Vec [a]
  deriving (Show, Eq, Ord, Foldable, Functor, Traversable)

type ET a = Typeable a

infix 4 %<, %<=, %>, %>=, %==, %/=

-- | Utility type used to embed a monadic computation in the monad `m` into a
-- language carrier `f`.
newtype Mon f m a = Mon {runMon :: forall b. Typeable b => (a -> f (m b)) -> f (m b)}
  deriving (Functor)

-- TODO: make this a proper distribution type
type Distr = Identity

-- | Type of shallow monadic embedding for `CPSFuzz`.
type CPSFuzzDistr a = Mon CPSFuzz Distr (CPSFuzz a)

-- | Type of shallow monadic embedding for `Expr`.
type ExprDistr a = Mon Expr Distr (Expr a)

-- | Syntax of order comparison.
class SynOrd a where
  type Carrier a :: * -> *

  (%<) :: a -> a -> Carrier a Bool
  (%<=) :: a -> a -> Carrier a Bool
  (%>) :: a -> a -> Carrier a Bool
  (%>=) :: a -> a -> Carrier a Bool
  (%==) :: a -> a -> Carrier a Bool
  (%/=) :: a -> a -> Carrier a Bool

class VecStorable a where
  -- | How many dimensions does it take to store `a`?
  vecSize :: Int

  fromVec :: Vec Number -> a
  asVec :: a -> Vec Number

class VecStorable a => VecMonoid a where
  -- | Law:
  -- 1. asVec (a `add` b) = (asVec a) + (asVec b) -- can distribute `asVec`
  -- 2. empty is identity for add
  -- 3. more?
  add :: a -> a -> a

  empty :: a

class VecMonoid a => Clip a where
  -- | Law:
  -- 1. asVec (clip r a) = map (clip r) (asVec a) -- can distribute `clip r`
  -- 2. clip r empty = empty
  -- 3. more?
  clip :: Vec Number -> a -> a

class Typeable (DeepRepr a) => Syntactic f a where
  type DeepRepr a :: *

  toDeepRepr :: a -> f (DeepRepr a)
  fromDeepRepr :: f (DeepRepr a) -> a

-- | A generic lambda calculus that can be executed on the client side.
--
--  TODO: Make this a closure-converted lambda calculus, so that all lambdas are
--  closed programs, and all captured variables are explicit arguments to
--  lambdas.
data Expr a :: * where
  EVar :: ET a => String -> Expr a
  ELam :: (ET a, ET b) => (Expr a -> Expr b) -> Expr (a -> b)
  EApp :: (ET a, ET b) => Expr (a -> b) -> Expr a -> Expr b
  EComp :: (ET a, ET b, ET c) => Expr (b -> c) -> Expr (a -> b) -> Expr (a -> c)
  EIf :: ET a => Expr Bool -> Expr a -> Expr a -> Expr a
  EIntLit :: Int -> Expr Int
  ENumLit :: Number -> Expr Number
  EAdd :: Expr Number -> Expr Number -> Expr Number
  EMinus :: Expr Number -> Expr Number -> Expr Number
  EMult :: Expr Number -> Expr Number -> Expr Number
  EDiv :: Expr Number -> Expr Number -> Expr Number
  EAbs :: Expr Number -> Expr Number
  EGT :: Expr Number -> Expr Number -> Expr Bool
  EGE :: Expr Number -> Expr Number -> Expr Bool
  ELT :: Expr Number -> Expr Number -> Expr Bool
  ELE :: Expr Number -> Expr Number -> Expr Bool
  EEQ :: Expr Number -> Expr Number -> Expr Bool
  ENEQ :: Expr Number -> Expr Number -> Expr Bool
  EJust :: ET a => Expr a -> Expr (Maybe a)
  ENothing :: ET a => Expr (Maybe a)
  EIsJust :: (ET a) => Expr (Maybe a) -> Expr Bool
  EFromJust :: (ET a) => Expr (Maybe a) -> Expr a
  {-
  EMonoidEmpty :: (VecMonoid a, ET a) => Expr a
  -- | Grow the vector by the given size.
  EVecExtend :: Expr Int -> Expr (Vec Number) -> Expr (Vec Number)
  -- | Focus on the vector in the given range [start, end).
  EVecFocus :: Expr Int -> Expr Int -> Expr (Vec Number) -> Expr (Vec Number)
  -- | Concatenate two vectors.
  EVecConcat :: Expr (Vec Number) -> Expr (Vec Number) -> Expr (Vec Number)
  -}
  EPair :: (ET a, ET b) => Expr a -> Expr b -> Expr (a, b)
  EFst :: (ET a, ET b) => Expr (a, b) -> Expr a
  ESnd :: (ET a, ET b) => Expr (a, b) -> Expr b
  EShare :: (ET a, ET b) => Expr a -> (Expr a -> Expr b) -> Expr b
  ELap :: Number -> Expr Number -> Expr (Distr Number)
  EReturn :: ET a => Expr a -> Expr (Distr a)
  EBind :: (ET a) => Expr (Distr a) -> (Expr a -> Expr (Distr b)) -> Expr (Distr b)

type CPSKont a r = CPSFuzz a -> CPSFuzz r

type Number = Double

type CFT a = (Typeable a)

type CFTM m = (Typeable m, Monad m)

data CPSFuzz (a :: *) where
  CVar :: CFT a => String -> CPSFuzz a
  CNumLit :: Number -> CPSFuzz Number
  -- We do not CPS these primitive operations.
  CAdd :: CPSFuzz Number -> CPSFuzz Number -> CPSFuzz Number
  CMinus :: CPSFuzz Number -> CPSFuzz Number -> CPSFuzz Number
  CMult :: CPSFuzz Number -> CPSFuzz Number -> CPSFuzz Number
  CDiv :: CPSFuzz Number -> CPSFuzz Number -> CPSFuzz Number
  CAbs :: CPSFuzz Number -> CPSFuzz Number
  CGT :: CPSFuzz Number -> CPSFuzz Number -> CPSFuzz Bool
  CGE :: CPSFuzz Number -> CPSFuzz Number -> CPSFuzz Bool
  CLT :: CPSFuzz Number -> CPSFuzz Number -> CPSFuzz Bool
  CLE :: CPSFuzz Number -> CPSFuzz Number -> CPSFuzz Bool
  CEQ :: CPSFuzz Number -> CPSFuzz Number -> CPSFuzz Bool
  CNEQ :: CPSFuzz Number -> CPSFuzz Number -> CPSFuzz Bool
  -- We only CPS bag operations.
  BMap :: (CFT a, CFT b, CFT r) => Expr (a -> b) -> CPSFuzz (Bag a) -> CPSKont (Bag b) r -> CPSFuzz r
  BSum :: CFT r => Vec Number -> CPSFuzz (Bag Number) -> CPSKont Number r -> CPSFuzz r
  -- | Avoid code explosion with explicit sharing.
  CShare :: (CFT a, CFT b) => CPSFuzz a -> (CPSFuzz a -> CPSFuzz b) -> CPSFuzz b
  -- | Deeply embed a monadic return.
  CReturn ::
    (CFT a) =>
    CPSFuzz a ->
    CPSFuzz (Distr a)
  -- | Deeply embed a monadic bind.
  CBind ::
    (CFT a, CFT b) =>
    CPSFuzz (Distr a) ->
    (CPSFuzz a -> CPSFuzz (Distr b)) ->
    CPSFuzz (Distr b)
  CLap :: Number -> CPSFuzz Number -> CPSFuzz (Distr Number)

type BT a = Typeable a

data BMCS (a :: *) where
  BVar :: BT a => String -> BMCS a
  BNumLit :: Number -> BMCS Number
  BReturn :: BT a => BMCS a -> BMCS (Distr a)
  BBind :: BT a => BMCS (Distr a) -> (BMCS a -> BMCS (Distr b)) -> BMCS (Distr b)
  BAdd :: BMCS Number -> BMCS Number -> BMCS Number
  BMinus :: BMCS Number -> BMCS Number -> BMCS Number
  BMult :: BMCS Number -> BMCS Number -> BMCS Number
  BDiv :: BMCS Number -> BMCS Number -> BMCS Number
  BAbs :: BMCS Number -> BMCS Number
  BGT :: BMCS Number -> BMCS Number -> BMCS Bool
  BGE :: BMCS Number -> BMCS Number -> BMCS Bool
  BLT :: BMCS Number -> BMCS Number -> BMCS Bool
  BLE :: BMCS Number -> BMCS Number -> BMCS Bool
  BEQ :: BMCS Number -> BMCS Number -> BMCS Bool
  BNEQ :: BMCS Number -> BMCS Number -> BMCS Bool
  BLap :: Number -> BMCS Number -> BMCS (Distr Number)
  Green :: BT a => BMCS a -> BMCS a
  Run ::
    (Typeable row, Typeable sum, Clip sum) =>
    -- | vector representation size
    Int ->
    -- | Clip bound with size equal to the representation size
    Vec Number ->
    -- | map function
    Expr (row -> sum) ->
    -- | release function
    Expr (sum -> Distr Number) ->
    BMCS (Distr Number)

-- ###############
-- # COMBINATORS #
-- ###############
bmap ::
  (CFT a, CFT b, CFT r) =>
  (Expr a -> Expr b) ->
  CPSFuzz (Bag a) ->
  (CPSFuzz (Bag b) -> CPSFuzz r) ->
  CPSFuzz r
bmap f bag k = BMap (ELam f) bag k

bfilter ::
  (CFT a, CFT r) =>
  (Expr a -> Expr Bool) ->
  CPSFuzz (Bag a) ->
  (CPSFuzz (Bag (Maybe a)) -> CPSFuzz r) ->
  CPSFuzz r
bfilter f bag k = bmap (\row -> eIf (f row) (eJust row) eNothing) bag k

bmapNothing ::
  (CFT a, CFT r) =>
  Expr a ->
  CPSFuzz (Bag (Maybe a)) ->
  (CPSFuzz (Bag a) -> CPSFuzz r) ->
  CPSFuzz r
bmapNothing r bag k = bmap (\row -> eIf (eIsJust row) (eFromJust row) r) bag k

bsum :: CFT r => Vec Number -> CPSFuzz (Bag Number) -> (CPSFuzz Number -> CPSFuzz r) -> CPSFuzz r
bsum bound bag k = BSum bound bag k

lap :: Number -> CPSFuzz Number -> CPSFuzzDistr Number
lap c w = fromDeepRepr $ CLap c w

-- | Give the value of `a` a temporary name, and use it in the body of `f`.
share :: (CFT a, CFT b, Syntactic CPSFuzz b) => CPSFuzz a -> (CPSFuzz a -> b) -> b
share a f = fromDeepRepr $ CShare a (toDeepRepr . f)

-- ##############
-- ## EXAMPLES ##
-- ##############

bag_filter_sum :: CPSFuzz (Bag Number) -> CPSFuzz Number
bag_filter_sum db =
  bfilter gt_10 db $
    \gt_10_db -> bmapNothing 0 gt_10_db $
      \gt_10_db -> bfilter lt_5 db $
        \lt_5_db -> bmapNothing 0 lt_5_db $
          \lt_5_db -> bsum 20 gt_10_db $
            \gt_10_sum -> bsum 5 lt_5_db $
              \lt_5_sum -> gt_10_sum + lt_5_sum
  where
    gt_10 :: Expr Number -> Expr Bool
    gt_10 v = v %> 10
    lt_5 :: Expr Number -> Expr Bool
    lt_5 v = v %< 5

bag_filter_sum_noise :: CPSFuzz (Bag Number) -> CPSFuzz (Distr Number)
bag_filter_sum_noise db = toDeepRepr
  $ share (bag_filter_sum db)
  $ \s -> do
    s1' <- lap 1.0 s
    s2' <- lap 2.0 s
    return (s1' + s2')

bag_filter_sum_noise2 :: CPSFuzz (Bag Number) -> CPSFuzz (Distr Number)
bag_filter_sum_noise2 db = toDeepRepr
  $ share (bag_filter_sum db)
  $ \s -> do
    s1' <- lap 1.0 s
    s2' <- lap 2.0 s1'
    return (s1' + s2')

bag_filter_sum_noise3 :: CPSFuzz (Bag Number) -> CPSFuzz (Distr Number)
bag_filter_sum_noise3 db = toDeepRepr
  $ share (bag_filter_sum db)
  $ \s -> do
    s1' <- lap 1.0 s
    s2' <- lap 2.0 (2 * s)
    return (s1' * s2')

bag_map_filter_sum :: CPSFuzz (Bag Number) -> CPSFuzz Number
bag_map_filter_sum db =
  bmap (* 2) db bag_filter_sum

-- ##################
-- # INFRASTRUCTURE #
-- ##################

fvCPSFuzz :: forall r. CPSFuzz r -> S.Set String -> S.Set String
fvCPSFuzz term inScope =
  flip evalState (nameState inScope) (fvCPSFuzz' term inScope)

fvCPSFuzz' :: forall r m. FreshM m => CPSFuzz r -> S.Set String -> m (S.Set String)
fvCPSFuzz' (CVar x) inScope =
  case x `S.member` inScope of
    True -> return S.empty
    False -> return $ S.singleton x
fvCPSFuzz' (CNumLit _) _ = return S.empty
fvCPSFuzz' (CAdd a b) inScope = do
  a' <- fvCPSFuzz' a inScope
  b' <- fvCPSFuzz' b inScope
  return $ S.union a' b'
fvCPSFuzz' (CMinus a b) inScope = do
  a' <- fvCPSFuzz' a inScope
  b' <- fvCPSFuzz' b inScope
  return $ S.union a' b'
fvCPSFuzz' (CMult a b) inScope = do
  a' <- fvCPSFuzz' a inScope
  b' <- fvCPSFuzz' b inScope
  return $ S.union a' b'
fvCPSFuzz' (CDiv a b) inScope = do
  a' <- fvCPSFuzz' a inScope
  b' <- fvCPSFuzz' b inScope
  return $ S.union a' b'
fvCPSFuzz' (CAbs a) inScope = fvCPSFuzz' a inScope
fvCPSFuzz' (CGT a b) inScope = do
  a' <- fvCPSFuzz' a inScope
  b' <- fvCPSFuzz' b inScope
  return $ S.union a' b'
fvCPSFuzz' (CGE a b) inScope = do
  a' <- fvCPSFuzz' a inScope
  b' <- fvCPSFuzz' b inScope
  return $ S.union a' b'
fvCPSFuzz' (CLT a b) inScope = do
  a' <- fvCPSFuzz' a inScope
  b' <- fvCPSFuzz' b inScope
  return $ S.union a' b'
fvCPSFuzz' (CLE a b) inScope = do
  a' <- fvCPSFuzz' a inScope
  b' <- fvCPSFuzz' b inScope
  return $ S.union a' b'
fvCPSFuzz' (CEQ a b) inScope = do
  a' <- fvCPSFuzz' a inScope
  b' <- fvCPSFuzz' b inScope
  return $ S.union a' b'
fvCPSFuzz' (CNEQ a b) inScope = do
  a' <- fvCPSFuzz' a inScope
  b' <- fvCPSFuzz' b inScope
  return $ S.union a' b'
fvCPSFuzz' (BMap _ db kont) inScope = do
  db' <- fvCPSFuzz' db inScope
  secretVarName <- gfresh "x"
  kont' <- fvCPSFuzz' (kont (CVar secretVarName)) (S.insert secretVarName inScope)
  return $ db' `S.union` kont'
fvCPSFuzz' (BSum _ db kont) inScope = do
  db' <- fvCPSFuzz' db inScope
  secretVarName <- gfresh "x"
  kont' <- fvCPSFuzz' (kont (CVar secretVarName)) (S.insert secretVarName inScope)
  return $ db' `S.union` kont'
fvCPSFuzz' (CShare a f) inScope = do
  a' <- fvCPSFuzz' a inScope
  secretVarName <- gfresh "x"
  f' <- fvCPSFuzz' (f (CVar secretVarName)) (S.insert secretVarName inScope)
  return $ a' `S.union` f'
fvCPSFuzz' (CReturn a) inScope =
  fvCPSFuzz' a inScope
fvCPSFuzz' (CBind a f) inScope = do
  a' <- fvCPSFuzz' a inScope
  secretVarName <- gfresh "x"
  f' <- fvCPSFuzz' (f (CVar secretVarName)) (S.insert secretVarName inScope)
  return $ a' `S.union` f'
fvCPSFuzz' (CLap _ c) inScope = fvCPSFuzz' c inScope

substBMCS :: forall a r. Typeable r => String -> BMCS a -> BMCS r -> BMCS a
substBMCS x term needle =
  case term of
    BVar y ->
      if x == y
        then case eqTypeRep (typeRep @a) (typeRep @r) of
          Just HRefl -> needle
          Nothing -> term
        else term
    BNumLit _ -> term
    BReturn m -> BReturn $ substBMCS x m needle
    BBind m f -> BBind (substBMCS x m needle) (\r -> substBMCS x (f r) needle)
    BAdd a b -> BAdd (substBMCS x a needle) (substBMCS x b needle)
    BMinus a b -> BMinus (substBMCS x a needle) (substBMCS x b needle)
    BMult a b -> BMult (substBMCS x a needle) (substBMCS x b needle)
    BDiv a b -> BDiv (substBMCS x a needle) (substBMCS x b needle)
    BAbs a -> BAbs (substBMCS x a needle)
    BGT a b -> BGT (substBMCS x a needle) (substBMCS x b needle)
    BGE a b -> BGE (substBMCS x a needle) (substBMCS x b needle)
    BLT a b -> BLT (substBMCS x a needle) (substBMCS x b needle)
    BLE a b -> BLE (substBMCS x a needle) (substBMCS x b needle)
    BEQ a b -> BEQ (substBMCS x a needle) (substBMCS x b needle)
    BNEQ a b -> BNEQ (substBMCS x a needle) (substBMCS x b needle)
    BLap c w -> BLap c (substBMCS x w needle)
    Green a -> Green (substBMCS x a needle)
    Run _ _ _ _ -> term

substCPSFuzz :: forall a r. Typeable r => String -> CPSFuzz a -> CPSFuzz r -> CPSFuzz a
substCPSFuzz x term needle =
  case term of
    CVar y ->
      if x == y
        then case eqTypeRep (typeRep @a) (typeRep @r) of
          Just HRefl -> needle
          Nothing -> term
        else term
    CNumLit _ -> term
    CAdd a b -> CAdd (substCPSFuzz x a needle) (substCPSFuzz x b needle)
    CMinus a b -> CMinus (substCPSFuzz x a needle) (substCPSFuzz x b needle)
    CMult a b -> CMult (substCPSFuzz x a needle) (substCPSFuzz x b needle)
    CDiv a b -> CDiv (substCPSFuzz x a needle) (substCPSFuzz x b needle)
    CAbs a -> CAbs (substCPSFuzz x a needle)
    CGT a b -> CGT (substCPSFuzz x a needle) (substCPSFuzz x b needle)
    CGE a b -> CGE (substCPSFuzz x a needle) (substCPSFuzz x b needle)
    CLT a b -> CLT (substCPSFuzz x a needle) (substCPSFuzz x b needle)
    CLE a b -> CLE (substCPSFuzz x a needle) (substCPSFuzz x b needle)
    CEQ a b -> CEQ (substCPSFuzz x a needle) (substCPSFuzz x b needle)
    CNEQ a b -> CNEQ (substCPSFuzz x a needle) (substCPSFuzz x b needle)
    -- f as an `Expr` cannot capture any variable from `CPSFuzz`, so we do not subst into f
    BMap f db kont -> BMap f (substCPSFuzz x db needle) (\r -> substCPSFuzz x (kont r) needle)
    BSum c db kont -> BSum c (substCPSFuzz x db needle) (\r -> substCPSFuzz x (kont r) needle)
    CShare a f -> CShare (substCPSFuzz x a needle) (\r -> substCPSFuzz x (f r) needle)
    CReturn a -> CReturn (substCPSFuzz x a needle)
    CBind m f -> CBind (substCPSFuzz x m needle) (\r -> substCPSFuzz x (f r) needle)
    CLap w c -> CLap w (substCPSFuzz x c needle)

substExpr :: forall a r. Typeable r => String -> Expr a -> Expr r -> Expr a
substExpr x term needle =
  case term of
    EVar y ->
      if x == y
        then case eqTypeRep (typeRep @a) (typeRep @r) of
          Just HRefl -> needle
          Nothing -> term
        else term
    ELam f -> ELam $ \r -> substExpr x (f r) needle
    EApp f a -> EApp (substExpr x f needle) (substExpr x a needle)
    EComp g f -> EComp (substExpr x g needle) (substExpr x f needle)
    EIf cond a b -> EIf (substExpr x cond needle) (substExpr x a needle) (substExpr x b needle)
    EIntLit _ -> term
    ENumLit _ -> term
    EAdd a b -> EAdd (substExpr x a needle) (substExpr x b needle)
    EMinus a b -> EMinus (substExpr x a needle) (substExpr x b needle)
    EMult a b -> EMult (substExpr x a needle) (substExpr x b needle)
    EDiv a b -> EDiv (substExpr x a needle) (substExpr x b needle)
    EAbs a -> EAbs (substExpr x a needle)
    EGT a b -> EGT (substExpr x a needle) (substExpr x b needle)
    EGE a b -> EGE (substExpr x a needle) (substExpr x b needle)
    ELT a b -> ELT (substExpr x a needle) (substExpr x b needle)
    ELE a b -> ELE (substExpr x a needle) (substExpr x b needle)
    EEQ a b -> EEQ (substExpr x a needle) (substExpr x b needle)
    ENEQ a b -> ENEQ (substExpr x a needle) (substExpr x b needle)
    EJust a -> EJust (substExpr x a needle)
    ENothing -> ENothing
    EIsJust a -> EIsJust (substExpr x a needle)
    EFromJust a -> EFromJust (substExpr x a needle)
    EPair a b -> EPair (substExpr x a needle) (substExpr x b needle)
    EFst a -> EFst (substExpr x a needle)
    ESnd a -> ESnd (substExpr x a needle)
    EShare a f -> EShare (substExpr x a needle) (\r -> substExpr x (f r) needle)
    ELap c w -> ELap c (substExpr x w needle)
    EReturn a -> EReturn (substExpr x a needle)
    EBind m f -> EBind (substExpr x m needle) (\r -> substExpr x (f r) needle)

instance SynOrd (Expr Number) where
  type Carrier (Expr Number) = Expr

  (%<) = ELT
  (%<=) = ELE
  (%>) = EGT
  (%>=) = EGE
  (%==) = EEQ
  (%/=) = ENEQ

instance Num (Expr Number) where
  (+) = EAdd
  (-) = EMinus
  (*) = EMult
  abs = EAbs
  signum v = (abs v) / v
  fromInteger = ENumLit . fromInteger

instance Fractional (Expr Number) where
  fromRational = ENumLit . fromRational
  (/) = EDiv

instance SynOrd (CPSFuzz Number) where
  type Carrier (CPSFuzz Number) = CPSFuzz

  (%<) = CLT
  (%<=) = CLE
  (%>) = CGT
  (%>=) = CGE
  (%==) = CEQ
  (%/=) = CNEQ

instance Num (CPSFuzz Number) where
  (+) = CAdd
  (-) = CMinus
  (*) = CMult
  abs = CAbs
  signum v = (abs v) / v
  fromInteger = CNumLit . fromInteger

instance Fractional (CPSFuzz Number) where
  fromRational = CNumLit . fromRational
  (/) = CDiv

vecConcat :: Vec a -> Vec a -> Vec a
vecConcat (Vec as) (Vec bs) = Vec (as ++ bs)

b2n :: Bool -> Number
b2n True = 1.0
b2n False = 0.0

infixl 9 %@, `eApp`

(%@) :: (ET a, ET b) => Expr (a -> b) -> Expr a -> Expr b
(%@) = eApp

eApp :: (ET a, ET b) => Expr (a -> b) -> Expr a -> Expr b
eApp = EApp

ecomp :: (ET a, ET b, ET c) => Expr (b -> c) -> Expr (a -> b) -> Expr (a -> c)
ecomp = EComp

eInt :: Int -> Expr Int
eInt = EIntLit

eNum :: Number -> Expr Number
eNum = ENumLit

eIf :: (ET a, Syntactic Expr a) => Expr Bool -> a -> a -> a
eIf cond t f = fromDeepRepr $ EIf cond (toDeepRepr t) (toDeepRepr f)

{-
eVecExtend :: Expr Int -> Expr (Vec Number) -> Expr (Vec Number)
eVecExtend = EVecExtend

eVecFocus :: Expr Int -> Expr Int -> Expr (Vec Number) -> Expr (Vec Number)
eVecFocus start end = EVecFocus start end

eVecConcat :: Expr (Vec Number) -> Expr (Vec Number) -> Expr (Vec Number)
eVecConcat = EVecConcat
-}

eFst :: (ET a, ET b) => Expr (a, b) -> Expr a
eFst = EFst

eSnd :: (ET a, ET b) => Expr (a, b) -> Expr b
eSnd = ESnd

eShare :: (ET a, ET b) => Expr a -> (Expr a -> Expr b) -> Expr b
eShare = EShare

eJust :: ET a => Expr a -> Expr (Maybe a)
eJust = EJust

eNothing :: ET a => Expr (Maybe a)
eNothing = ENothing

eIsJust :: ET a => Expr (Maybe a) -> Expr Bool
eIsJust = EIsJust

eFromJust :: ET a => Expr (Maybe a) -> Expr a
eFromJust = EFromJust

{-
eMonoidEmpty :: (ET a, VecMonoid a) => Expr a
eMonoidEmpty = EMonoidEmpty
-}

instance Num (Vec Number) where
  (Vec as) + (Vec bs) = Vec (zipWith (+) as bs)
  (Vec as) - (Vec bs) = Vec (zipWith (-) as bs)
  (Vec as) * (Vec bs) = Vec (zipWith (*) as bs)
  abs (Vec as) = Vec (map abs as)
  signum _ = error "signum: Vec Number has no signum"
  fromInteger x = Vec [fromInteger x] --error "fromInteger: Vec Number cannot be constructed from Integer"

instance Fractional (Vec Number) where
  (Vec as) / (Vec bs) = Vec (zipWith (/) as bs)
  fromRational x = Vec [fromRational x] --error "fromRational: Vec Number cannot be constructed from Rational"

instance VecStorable Number where
  vecSize = 1
  fromVec (Vec (x : _)) = x
  fromVec _ = error "fromVec<Number>: cannot restore from empty vec"
  asVec a = Vec [a]

instance (VecStorable a, VecStorable b) => VecStorable (a, b) where
  vecSize = vecSize @a + vecSize @b
  fromVec (Vec as) =
    (fromVec (Vec $ take (vecSize @a) as), fromVec (Vec . take (vecSize @b) . drop (vecSize @a) $ as))
  asVec (a, b) = vecConcat (asVec a) (asVec b)

instance VecMonoid a => VecStorable (Maybe a) where
  vecSize = 1 + vecSize @a
  fromVec (Vec []) = error "fromVec: cannot decode (Maybe a) from empty vector"
  fromVec (Vec (x : more)) =
    if x > 0
      then Just (fromVec (Vec more))
      else Nothing
  asVec (Just a) =
    let Vec a' = asVec a
     in Vec (1 : a')
  asVec Nothing =
    let Vec a' = asVec (empty @a)
     in Vec (0 : a')

instance VecMonoid Number where
  add = (+)
  empty = 0

instance (VecMonoid a, VecMonoid b) => VecMonoid (a, b) where
  add (a1, b1) (a2, b2) = (add a1 a2, add b1 b2)
  empty = (empty @a, empty @b)

instance VecMonoid a => VecMonoid (Maybe a) where
  add (Just a) (Just b) = Just (a `add` b)
  add (Just a) Nothing = Just a
  add Nothing (Just b) = Just b
  add Nothing Nothing = Nothing
  empty = Nothing

instance Clip Number where
  clip (Vec [r]) a =
    if a >= bound then bound else if a <= - bound then - bound else a
    where
      bound = abs r
  clip _ _ = error "clip @Number: expect bound to have size 1"

instance (Clip a, Clip b) => Clip (a, b) where
  clip r (a, b) = (clip r a, clip r b)

instance Clip a => Clip (Maybe a) where
  clip r = fmap (clip r)

-- | Manual and deferrable resolution of the `Clip` constraint for any typeable type.
resolveClip :: forall (a :: *). Typeable a => Maybe (Dict (Clip a))
resolveClip =
  case eqTypeRep (typeRep @Number) (typeRep @a) of
    Just HRefl -> Just Dict
    _ -> do
      case (typeRep @a) of
        App maybeMaybe (a1 :: typeRep a1)
          | SomeTypeRep maybeMaybe == SomeTypeRep (typeRep @Maybe) -> do
            HRefl <- eqTypeRep (typeRepKind a1) (typeRepKind (typeRep @Int))
            a1Dict <- withTypeable a1 $ resolveClip @a1
            case eqTypeRep maybeMaybe (typeRep @Maybe) of
              Just HRefl -> withDict a1Dict $ return (Dict @(Clip (Maybe a1)))
              _ -> Nothing
        App con (a2 :: typeRep a2) -> do
          HRefl <- eqTypeRep (typeRepKind a2) (typeRepKind (typeRep @Int))
          (a2Dict :: Dict (Clip a2)) <- withTypeable a2 $ resolveClip @a2
          case con of
            App maybeT2 (a1 :: TypeRep a1)
              | SomeTypeRep maybeT2 == SomeTypeRep (typeRep @(,)) -> do
                HRefl <- eqTypeRep (typeRepKind a1) (typeRepKind (typeRep @Int))
                HRefl <- eqTypeRep (maybeT2) (typeRep @(,))
                (a1Dict :: Dict (Clip a1)) <- withTypeable a1 $ resolveClip @a1
                let tupleDict = withDict a1Dict $ withDict a2Dict $ Dict @(Clip (a1, a2))
                return tupleDict
            _ -> Nothing
        _ -> Nothing

instance Applicative (Mon f m) where
  pure a = Mon $ \k -> k a
  f <*> a = f >>= \f' -> a >>= \a' -> return (f' a')

instance Monad (Mon f m) where
  return a = Mon $ \k -> k a
  Mon m >>= f = Mon $ \k -> m (\a -> runMon (f a) k)

-- ###########
-- ## MAGIC ##
-- ###########

instance ET a => Syntactic Expr (Expr a) where
  type DeepRepr (Expr a) = a

  toDeepRepr = id
  fromDeepRepr = id

instance CFT a => Syntactic CPSFuzz (CPSFuzz a) where
  type DeepRepr (CPSFuzz a) = a

  toDeepRepr = id
  fromDeepRepr = id

instance
  ( Syntactic Expr a,
    Syntactic Expr b
  ) =>
  Syntactic Expr (a -> b)
  where
  type DeepRepr (a -> b) = DeepRepr a -> DeepRepr b

  toDeepRepr f = ELam $ toDeepRepr . f . fromDeepRepr
  fromDeepRepr f = fromDeepRepr . EApp f . toDeepRepr

instance
  ( Syntactic Expr a,
    Syntactic Expr b
  ) =>
  Syntactic Expr (a, b)
  where
  type DeepRepr (a, b) = (DeepRepr a, DeepRepr b)

  toDeepRepr (a, b) = EPair (toDeepRepr a) (toDeepRepr b)
  fromDeepRepr p = (fromDeepRepr (EFst p), fromDeepRepr (ESnd p))

instance
  ( Syntactic Expr a,
    CFT (DeepRepr a)
  ) =>
  Syntactic Expr (Mon Expr Distr a)
  where
  type DeepRepr (Mon Expr Distr a) = Distr (DeepRepr a)
  toDeepRepr (Mon m) = m (EReturn . toDeepRepr)
  fromDeepRepr m = Mon $ \k -> EBind m (k . fromDeepRepr)

instance
  ( Syntactic CPSFuzz a,
    CFT (DeepRepr a)
  ) =>
  Syntactic CPSFuzz (Mon CPSFuzz Distr a)
  where
  type DeepRepr (Mon CPSFuzz Distr a) = Distr (DeepRepr a)
  toDeepRepr (Mon m) = m (CReturn . toDeepRepr)
  fromDeepRepr m = Mon $ \k -> CBind m (k . fromDeepRepr)
