{-# LANGUAGE AllowAmbiguousTypes #-}

module Lib where

import Data.Functor.Identity
import IfCxt
import Type.Reflection

newtype Bag a = Bag [a]
  deriving (Show, Eq, Ord)

newtype Vec a = Vec [a]
  deriving (Show, Eq, Ord)

type ET a = Typeable a

infix 4 %<, %<=, %>, %>=, %==, %/=

-- | Utility type used to embed a monadic computation in the monad `m` into a
-- language carrier `f`.
newtype Mon f m a = Mon {runMon :: forall b. (a -> f (m b)) -> f (m b)}
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
  clip :: Number -> a -> a

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
  EMonoidEmpty :: (VecMonoid a, ET a) => Expr a
  -- | Grow the vector by the given size.
  EVecExtend :: Expr Int -> Expr (Vec Number) -> Expr (Vec Number)
  -- | Focus on the vector in the given range [start, end).
  EVecFocus :: Expr Int -> Expr Int -> Expr (Vec Number) -> Expr (Vec Number)
  -- | Concatenate two vectors.
  EVecConcat :: Expr (Vec Number) -> Expr (Vec Number) -> Expr (Vec Number)
  EPair :: (ET a, ET b) => Expr a -> Expr b -> Expr (a, b)
  EFst :: (ET a, ET b) => Expr (a, b) -> Expr a
  ESnd :: (ET a, ET b) => Expr (a, b) -> Expr b
  EShare :: (ET a, ET b) => Expr a -> (Expr a -> Expr b) -> Expr b
  ELap :: Number -> Expr Number -> Expr (Distr Number)
  EReturn :: Expr a -> Expr (Distr a)
  EBind :: Expr (Distr a) -> (Expr a -> Expr (Distr b)) -> Expr (Distr b)

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
  BFilter :: (CFT a, CFT r, VecMonoid a) => Expr (a -> Bool) -> CPSFuzz (Bag a) -> CPSKont (Bag a) r -> CPSFuzz r
  BSum :: CFT r => Number -> CPSFuzz (Bag Number) -> CPSKont Number r -> CPSFuzz r
  -- | Avoid code explosion with explicit sharing.
  CShare :: (CFT a, CFT b) => CPSFuzz a -> (CPSFuzz a -> CPSFuzz b) -> CPSFuzz b
  -- | Deeply embed a monadic return.
  CReturn ::
    (CFT a) =>
    CPSFuzz a ->
    CPSFuzz (Distr a)
  -- | Deeply embed a monadic bind.
  CBind ::
    (CFT a) =>
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
  {- TODO: we need something like this eventually, but this may not be the right way to do it.
  -- |Run a green-zone computation on non-sensitive data using unrestricted language.
  Green   :: (BT a, BT b) => Expr (a -> b) -> BMCS (a -> b)
  -}

  Run ::
    -- | vector representation size
    Int ->
    -- | Clip bound
    Number ->
    -- | map function
    Expr (Vec Number -> Vec Number) ->
    -- | release function
    Expr (Vec Number -> Distr Number) ->
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
  (CFT a, CFT r, VecMonoid a) =>
  (Expr a -> Expr Bool) ->
  CPSFuzz (Bag a) ->
  (CPSFuzz (Bag a) -> CPSFuzz r) ->
  CPSFuzz r
bfilter f bag k = BFilter (ELam f) bag k

bsum :: CFT r => Number -> CPSFuzz (Bag Number) -> (CPSFuzz Number -> CPSFuzz r) -> CPSFuzz r
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
    \gt_10_db -> bfilter lt_5 db $
      \lt_5_db -> bsum 20 gt_10_db $
        \gt_10_sum -> bsum 5 lt_5_db $
          \lt_10_sum -> gt_10_sum + lt_10_sum
  where
    gt_10 :: Expr Number -> Expr Bool
    gt_10 v = v %> 10
    lt_5 :: Expr Number -> Expr Bool
    lt_5 v = v %< 5

bag_filter_sum_noise :: CPSFuzz (Bag Number) -> CPSFuzzDistr Number
bag_filter_sum_noise db =
  share (bag_filter_sum db) $
    \s -> do
      s1' <- lap 1.0 s
      s2' <- lap 2.0 s
      return (s1' + s2')

bag_map_filter_sum :: CPSFuzz (Bag Number) -> CPSFuzz Number
bag_map_filter_sum db =
  bmap (* 2) db bag_filter_sum

-- ##################
-- # INFRASTRUCTURE #
-- ##################

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

secretVarName :: String
secretVarName = "do not create a variable with this name... otherwise everything falls apart :)"

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

eVecExtend :: Expr Int -> Expr (Vec Number) -> Expr (Vec Number)
eVecExtend = EVecExtend

eVecFocus :: Expr Int -> Expr Int -> Expr (Vec Number) -> Expr (Vec Number)
eVecFocus start end = EVecFocus start end

eVecConcat :: Expr (Vec Number) -> Expr (Vec Number) -> Expr (Vec Number)
eVecConcat = EVecConcat

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

eMonoidEmpty :: (ET a, VecMonoid a) => Expr a
eMonoidEmpty = EMonoidEmpty

instance Num (Vec Number) where
  (Vec as) + (Vec bs) = Vec (zipWith (+) as bs)
  (Vec as) - (Vec bs) = Vec (zipWith (-) as bs)
  (Vec as) * (Vec bs) = Vec (zipWith (*) as bs)
  abs (Vec as) = Vec (map abs as)
  signum _ = error "signum: Vec Number has no signum"
  fromInteger _ = error "fromInteger: Vec Number cannot be constructed from Integer"

instance Fractional (Vec Number) where
  (Vec as) / (Vec bs) = Vec (zipWith (/) as bs)
  fromRational _ = error "fromRational: Vec Number cannot be constructed from Rational"

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
  clip r a =
    if a >= bound then bound else if a <= - bound then - bound else a
    where
      bound = abs r

instance (Clip a, Clip b) => Clip (a, b) where
  clip r (a, b) = (clip r a, clip r b)

instance Clip a => Clip (Maybe a) where
  clip r = fmap (clip r)

mkIfCxtInstances ''VecStorable

mkIfCxtInstances ''VecMonoid

mkIfCxtInstances ''Clip

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
