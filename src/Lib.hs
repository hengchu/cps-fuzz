{-# LANGUAGE AllowAmbiguousTypes #-}

module Lib where

import Type.Reflection

newtype Bag a = Bag [a]
  deriving (Show, Eq, Ord)
newtype Vec a = Vec [a]
  deriving (Show, Eq, Ord)

type ET a = Typeable a

infix 4 %<, %<=, %>, %>=, %==, %/=

-- |Syntax of order comparison.
class SynOrd a where
  type Carrier a :: * -> *

  (%<)   :: a -> a -> Carrier a Bool
  (%<=)  :: a -> a -> Carrier a Bool
  (%>)   :: a -> a -> Carrier a Bool
  (%>=)  :: a -> a -> Carrier a Bool
  (%==)  :: a -> a -> Carrier a Bool
  (%/=)  :: a -> a -> Carrier a Bool

class VecStorable a where
  -- |How many dimensions does it take to store `a`?
  vecSize :: Int

  fromVec :: Vec Number -> a
  asVec   :: a -> Vec Number

-- |A generic lambda calculus that can be executed on the client side.
--
-- TODO: Make this a closure-converted lambda calculus, so that all lambdas are
-- closed programs, and all captured variables are explicit arguments to
-- lambdas.
data Expr a :: * where
  EVar    ::                       String             -> Expr a
  ELam    :: (ET a, ET b)       => (Expr a -> Expr b) -> Expr (a -> b)
  EApp    :: (ET a, ET b)       => Expr (a -> b)      -> Expr a -> Expr b
  EComp   :: (ET a, ET b, ET c) => Expr (b -> c)      -> Expr (a -> b) -> Expr (a -> c)
  EIf     :: ET a               => Expr Bool          -> Expr a        -> Expr a -> Expr a

  EIntLit :: Int    -> Expr Int
  ENumLit :: Number -> Expr Number

  EBoolToNum :: Expr Bool -> Expr Number

  -- |Client-side vector manipulation functions.
  EFocus     :: Expr Int -> Expr Int -> Expr (Vec Number -> Vec Number)
  EVecSum    :: Expr (Vec Number -> Number)
  EVecExtend :: Expr Int -> Expr (Vec Number -> Vec Number)
  EVecStore  :: Expr Int -> Expr Int -- read start, read end
             -> Expr Int -> Expr Int -- write start, write end
             -> Expr (Vec Number -> Vec Number) -- function applied to read part
             -> Expr (Vec Number -> Vec Number)
  EVecZeros  :: Expr Int -> Expr (Vec Number)
  EAsVec   :: (ET a, VecStorable a) => Expr a -> Expr (Vec Number)
  EFromVec :: (ET a, VecStorable a) => Expr (Vec Number) -> Expr a

  EAdd   :: Expr Number -> Expr Number -> Expr Number
  EMinus :: Expr Number -> Expr Number -> Expr Number
  EMult  :: Expr Number -> Expr Number -> Expr Number
  EDiv   :: Expr Number -> Expr Number -> Expr Number
  EAbs   :: Expr Number -> Expr Number

  EGT  :: Expr Number -> Expr Number -> Expr Bool
  EGE  :: Expr Number -> Expr Number -> Expr Bool
  ELT  :: Expr Number -> Expr Number -> Expr Bool
  ELE  :: Expr Number -> Expr Number -> Expr Bool
  EEQ  :: Expr Number -> Expr Number -> Expr Bool
  ENEQ :: Expr Number -> Expr Number -> Expr Bool

type CPSKont a r = CPSFuzz a -> CPSFuzz r
type Number      = Double

type CFT a = (Typeable a, VecStorable a)

data CPSFuzz (a :: *) where
  CVar    :: String -> CPSFuzz a
  CNumLit :: Number -> CPSFuzz Number

  -- We do not CPS these primitive operations.
  CAdd   :: CPSFuzz Number -> CPSFuzz Number -> CPSFuzz Number
  CMinus :: CPSFuzz Number -> CPSFuzz Number -> CPSFuzz Number
  CMult  :: CPSFuzz Number -> CPSFuzz Number -> CPSFuzz Number
  CDiv   :: CPSFuzz Number -> CPSFuzz Number -> CPSFuzz Number
  CAbs   :: CPSFuzz Number -> CPSFuzz Number

  CGT  :: CPSFuzz Number -> CPSFuzz Number -> CPSFuzz Bool
  CGE  :: CPSFuzz Number -> CPSFuzz Number -> CPSFuzz Bool
  CLT  :: CPSFuzz Number -> CPSFuzz Number -> CPSFuzz Bool
  CLE  :: CPSFuzz Number -> CPSFuzz Number -> CPSFuzz Bool
  CEQ  :: CPSFuzz Number -> CPSFuzz Number -> CPSFuzz Bool
  CNEQ :: CPSFuzz Number -> CPSFuzz Number -> CPSFuzz Bool

  -- We only CPS bag operations.
  BMap    :: (CFT a, CFT b) => Expr (a -> b)    -> CPSFuzz (Bag a)      -> CPSKont (Bag b) r -> CPSFuzz r
  BFilter :: CFT a          => Expr (a -> Bool) -> CPSFuzz (Bag a)      -> CPSKont (Bag a) r -> CPSFuzz r
  BSum    ::                   Number           -> CPSFuzz (Bag Number) -> CPSKont Number  r -> CPSFuzz r

type BT a = Typeable a

data BMCS (a :: *) where
  BVar    :: String -> BMCS a
  BNumLit :: Number -> BMCS Number
  Run :: Number                          -- ^clip bound
      -> Int                             -- ^vector representation size
      -> Expr (Vec Number -> Vec Number) -- ^map function
      -> Expr (Vec Number -> Number)     -- ^release function
      -> BMCS Number

-- ###############
-- # COMBINATORS #
-- ###############
bmap :: (CFT a, CFT b, CFT r)
     => (Expr a -> Expr b)
     -> CPSFuzz (Bag a)
     -> (CPSFuzz (Bag b) -> CPSFuzz r)
     -> CPSFuzz r
bmap f bag k = BMap (ELam f) bag k

bfilter :: (CFT a, CFT r)
        => (Expr a -> Expr Bool)
        -> CPSFuzz (Bag a)
        -> (CPSFuzz (Bag a) -> CPSFuzz r)
        -> CPSFuzz r
bfilter f bag k = BFilter (ELam f) bag k

bsum :: CFT r => Number -> CPSFuzz (Bag Number) -> (CPSFuzz Number -> CPSFuzz r) -> CPSFuzz r
bsum bound bag k = BSum bound bag k

-- ##############
-- ## EXAMPLES ##
-- ##############

bag_filter_sum :: CPSFuzz (Bag Number) -> CPSFuzz Number
bag_filter_sum db =
  bfilter gt_10 db $
  \gt_10_db  -> bfilter lt_5 db  $
  \lt_5_db   -> bsum 20 gt_10_db $
  \gt_10_sum -> bsum 5  lt_5_db  $
  \lt_10_sum -> gt_10_sum + lt_10_sum
  where
    gt_10 :: Expr Number -> Expr Bool
    gt_10 v = v %> 10

    lt_5 :: Expr Number -> Expr Bool
    lt_5 v = v %< 5

bag_map_filter_sum :: CPSFuzz (Bag Number) -> CPSFuzz Number
bag_map_filter_sum db =
  bmap (*2) db bag_filter_sum

-- ##################
-- # INFRASTRUCTURE #
-- ##################

instance SynOrd (Expr Number) where
  type Carrier (Expr Number) = Expr

  (%<)  = ELT
  (%<=) = ELE
  (%>)  = EGT
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

  (%<)  = CLT
  (%<=) = CLE
  (%>)  = CGT
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

{-
substExpr :: forall a r. (BT a, BT r) => String -> Expr a -> Expr r -> Expr r
substExpr x term e = undefined

substCPSFuzz :: forall a r. (BT a, BT r) => String -> CPSFuzz a -> CPSFuzz r -> CPSFuzz r
substCPSFuzz x term e = undefined

substBMCS :: forall a r. (BT a, BT r) => String -> BMCS a -> BMCS r -> BMCS r
substBMCS x term e = undefined
-}

vecConcat :: Vec a -> Vec a -> Vec a
vecConcat (Vec as) (Vec bs) = Vec (as ++ bs)

vecSum :: Vec Number -> Number
vecSum (Vec as) = sum as

vecExtend :: Int -> Vec Number -> Vec Number
vecExtend w (Vec as) = Vec (as ++ take w (repeat 0))

vecStore :: Int -> Int -> Int -> Int
         -> (Vec Number -> Vec Number)
         ->  Vec Number -> Vec Number
vecStore readStart readEnd writeStart writeEnd f (Vec as) =
  let input = take (readEnd - readStart + 1) (drop readStart as)
      Vec output = f (Vec input)
      left  = take writeStart as
      right = drop (writeEnd+1) as
  in if length input /= readEnd - readStart + 1
     then error "vecStore: input length calculation wrong"
     else if length output /= writeEnd - writeStart + 1
          then error "vecStore: output length calculation wrong"
          else Vec (left ++ output ++ right)

vecZeros :: Int -> Vec Number
vecZeros n = Vec $ take n (repeat 0)

focus :: Int -> Int -> Vec Number -> Vec Number
focus start end (Vec as) =
  Vec . take (end - start + 1) . drop start $ as

b2n :: Bool -> Number
b2n True  = 1.0
b2n False = 0.0

eLam :: (ET a, ET b) => (Expr a -> Expr b) -> Expr (a -> b)
eLam = ELam

eApp :: (ET a, ET b) => Expr (a -> b) -> Expr a -> Expr b
eApp = EApp

ecomp :: (ET a, ET b, ET c) => Expr (b -> c) -> Expr (a -> b) -> Expr (a -> c)
ecomp = EComp

eInt :: Int -> Expr Int
eInt = EIntLit

eNum :: Number -> Expr Number
eNum = ENumLit

eVecSum :: Expr (Vec Number -> Number)
eVecSum = EVecSum

eVecExtend :: Expr Int -> Expr (Vec Number -> Vec Number)
eVecExtend = EVecExtend

eVecStore :: Expr Int -> Expr Int
          -> Expr Int -> Expr Int
          -> Expr (Vec Number -> Vec Number)
          -> Expr (Vec Number -> Vec Number)
eVecStore = EVecStore

eAsVec :: (ET a, VecStorable a) => Expr a -> Expr (Vec Number)
eAsVec = EAsVec

eAsVecPF :: (ET a, VecStorable a) => Expr (a -> Vec Number)
eAsVecPF = eLam eAsVec

eFromVec :: (ET a, VecStorable a) => Expr (Vec Number) -> Expr a
eFromVec = EFromVec

eFromVecPF :: (ET a, VecStorable a) => Expr (Vec Number -> a)
eFromVecPF = eLam eFromVec

eVecZeros :: Expr Int -> Expr (Vec Number)
eVecZeros = EVecZeros

eIf :: ET a => Expr Bool -> Expr a -> Expr a -> Expr a
eIf = EIf

eFocus :: Expr Int -> Expr Int -> Expr (Vec Number -> Vec Number)
eFocus = EFocus

eB2N :: Expr Bool -> Expr Number
eB2N = EBoolToNum

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
  fromVec (Vec (x:_)) = x
  fromVec _ = error "fromVec<Number>: cannot restore from empty vec"
  asVec a = Vec [a]

instance (VecStorable a, VecStorable b) => VecStorable (a, b) where
  vecSize = vecSize @a + vecSize @b
  fromVec (Vec as) =
    (fromVec (Vec $ take (vecSize @a) as), fromVec (Vec . take (vecSize @b) . drop (vecSize @a) $ as))
  asVec (a, b) = vecConcat (asVec a) (asVec b)
