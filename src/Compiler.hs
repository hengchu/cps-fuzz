module Compiler where

import Control.Lens
import Control.Monad.Catch
import Control.Monad.State.Strict
import Data.Proxy
import Data.Semigroup
import IfCxt
import Lib
import Names
import Type.Reflection

data AnyMF :: * where
  AnyMF ::
    ( ET a,
      ET b,
      IfCxt (VecMonoid b),
      IfCxt (Clip b)
    ) =>
    Expr (a -> b) ->
    AnyMF

data AnyRF :: * where
  AnyRF ::
    ( ET a,
      ET b,
      IfCxt (VecMonoid a),
      IfCxt (Clip a)
    ) =>
    Expr (a -> Distr b) ->
    AnyRF

data Effects
  = Effects
      { _eDbName :: String,
        _eClipBound :: Maybe Number,
        _eMapFunction :: AnyMF,
        _eReleaseFunction :: AnyRF
      }

makeLensesWith abbreviatedFields ''Effects

data TypedEffects a b r
  = TypedEffects
      { _teDbName :: String,
        _teClipBound :: Maybe Number,
        _teMapFunction :: Expr (a -> b),
        _teReleaseFunction :: Expr (b -> Distr r)
      }

makeLensesWith abbreviatedFields ''TypedEffects

data Compiled a :: * where
  Effectful ::
    ( Typeable row,
      Typeable aggr,
      Typeable r
    ) =>
    TypedEffects row aggr r ->
    Compiled r
  Pure ::
    Typeable a =>
    Expr a ->
    Compiled a

{-
mkEffect :: Maybe Number -> AnyMF -> AnyRF -> Compiled Number
mkEffect bound m r = Effectful $ Effects bound m r
-}

data CompilerError
  = InternalError String
  | -- | We need a type that satisfies `VecMonoid`
    --  but instead got this.
    RequiresVecMonoid SomeTypeRep
  | -- | We need a type that satisfies `Clip`, but
    --  instead got this.
    RequiresClip SomeTypeRep
  | TypeError {expected :: SomeTypeRep, observed :: SomeTypeRep}
  deriving (Show, Eq, Ord, Typeable)

newtype Compiler a = Compiler {runCompiler_ :: StateT NameState (Either SomeException) a}
  deriving
    (Functor, Applicative, Monad, MonadThrow, MonadState NameState)
    via (StateT NameState (Either SomeException))

runCompiler :: Compiler a -> Either SomeException a
runCompiler = flip evalStateT emptyNameState . runCompiler_

typecheck ::
  forall a b r m.
  ( MonadThrow m,
    Typeable a,
    Typeable b,
    Typeable r
  ) =>
  Effects ->
  m (TypedEffects a b r)
typecheck
  ( Effects
      dbName
      bound
      (AnyMF (mf :: Expr (a1 -> b1)))
      (AnyRF (rf :: Expr (c1 -> Distr r1)))
    ) =
    case eqTypeRep (typeRep @a) (typeRep @a1) of
      Nothing -> throwM $ TypeError (SomeTypeRep (typeRep @a)) (SomeTypeRep (typeRep @a1))
      Just HRefl ->
        case eqTypeRep (typeRep @b) (typeRep @b1) of
          Nothing -> throwM $ TypeError (SomeTypeRep (typeRep @b)) (SomeTypeRep (typeRep @b1))
          Just HRefl ->
            case eqTypeRep (typeRep @b) (typeRep @c1) of
              Nothing -> throwM $ TypeError (SomeTypeRep (typeRep @b)) (SomeTypeRep (typeRep @c1))
              Just HRefl ->
                case eqTypeRep (typeRep @r) (typeRep @r1) of
                  Nothing -> throwM $ TypeError (SomeTypeRep (typeRep @r)) (SomeTypeRep (typeRep @r1))
                  Just HRefl ->
                    return $ TypedEffects dbName bound mf rf

typecheck' ::
  forall a b c m r.
  ( MonadThrow m,
    Typeable a,
    Typeable b,
    Typeable c,
    IfCxt (VecMonoid b),
    IfCxt (Clip b)
  ) =>
  Effects ->
  ((VecMonoid b, Clip b) => TypedEffects a b c -> m r) ->
  m r
typecheck' ef kont = do
  typedEf <- typecheck @a @b @c ef
  ifCxt
    (Proxy :: Proxy (VecMonoid b))
    ( ifCxt
        (Proxy :: Proxy (Clip b))
        (kont typedEf)
        (throwM $ RequiresClip (SomeTypeRep (typeRep @b)))
    )
    (throwM $ RequiresVecMonoid (SomeTypeRep (typeRep @b)))

erasetypes ::
  ( Typeable a,
    Typeable b,
    Typeable r,
    IfCxt (VecMonoid b),
    IfCxt (Clip b),
    IfCxt (VecStorable r)
  ) =>
  TypedEffects a b r ->
  Effects
erasetypes ef =
  Effects
    (ef ^. dbName)
    (ef ^. clipBound)
    (AnyMF $ ef ^. mapFunction)
    (AnyRF $ ef ^. releaseFunction)

joinBounds :: Maybe Number -> Maybe Number -> Maybe Number
joinBounds a b = fmap getMin $ (fmap Min a) <> (fmap Min b)

simdCombine ::
  forall r1 r2 r a b1 b2 m.
  ( MonadThrow m,
    ET r1,
    ET r2,
    ET r,
    ET a,
    ET b1,
    ET b2
  ) =>
  TypedEffects a b1 r1 ->
  TypedEffects a b2 r2 ->
  (Expr r1 -> Expr r2 -> Expr r) ->
  m (TypedEffects a (b1, b2) r)
simdCombine
  (TypedEffects dbName1 bound1 mf1 rf1)
  (TypedEffects dbName2 bound2 mf2 rf2)
  combine = do
    when (dbName1 /= dbName2) $ do
      throwM . InternalError $
        "simdCombine: expected both effects to operate over the same input db, but got "
          ++ dbName1
          ++ ", and "
          ++ dbName2
    let mf1' = (fromDeepRepr mf1) :: (Expr a -> Expr b1)
    let mf2' = (fromDeepRepr mf2) :: (Expr a -> Expr b2)
    let mf = toDeepRepr @Expr $ \row -> (mf1' row, mf2' row)
    let rf1' = (fromDeepRepr rf1) :: (Expr b1 -> ExprDistr r1)
    let rf2' = (fromDeepRepr rf2) :: (Expr b2 -> ExprDistr r2)
    let rf = toDeepRepr @Expr $ \(b1, b2) -> do
          r1' <- rf1' b1
          r2' <- rf2' b2
          return $ combine r1' r2'
    return $ TypedEffects dbName1 (joinBounds bound1 bound2) mf rf

postprocess ::
  forall r1 r2 a b m.
  ( ET r1,
    ET r2,
    ET a,
    ET b,
    MonadThrow m
  ) =>
  TypedEffects a b r1 ->
  (Expr r1 -> Expr r2) ->
  m (TypedEffects a b r2)
postprocess (TypedEffects dbName bound mf rf) f = do
  let rf' = (fromDeepRepr rf) :: (Expr b -> ExprDistr r1)
  let newRf = \aggr -> do
        aggr' <- rf' aggr
        return (f aggr')
  return $ TypedEffects dbName bound mf (toDeepRepr newRf)

compileBinop' ::
  ( MonadThrow m,
    FreshM m,
    Typeable r,
    Typeable a,
    Typeable b
  ) =>
  CPSFuzz a ->
  CPSFuzz b ->
  (Expr a -> Expr b -> Expr r) ->
  m (Compiled r)
compileBinop' a b combine = do
  a' <- compile' a
  b' <- compile' b
  case (a', b') of
    (Pure pa, Pure pb) -> return $ Pure (combine pa pb)
    (Pure pa, Effectful eb) -> Effectful <$> postprocess eb (combine pa)
    (Effectful ea, Pure pb) -> Effectful <$> postprocess ea (`combine` pb)
    (Effectful (ea :: TypedEffects row1 _ _), Effectful (eb :: TypedEffects row2 _ _)) ->
      case eqTypeRep (typeRep @row1) (typeRep @row2) of
        Just HRefl ->
          Effectful <$> simdCombine ea eb combine
        Nothing ->
          throwM $ TypeError (SomeTypeRep (typeRep @row1)) (SomeTypeRep (typeRep @row2))

-- | Given a `CPSFuzz` program, this function returns the effectful computation
--  (if any), and a `BMCS` term that would evaluate to the result of the
--  `CPSFuzz` program's result.
compile' ::
  forall r m.
  ( MonadThrow m,
    FreshM m
  ) =>
  CPSFuzz r ->
  m (Compiled r)
compile' (CVar x) = return (Pure (EVar x))
compile' (CNumLit v) = return (Pure (ENumLit v))
compile' (CAdd a b) = compileBinop' a b (+)
compile' (CMinus a b) = compileBinop' a b (-)
compile' (CMult a b) = compileBinop' a b (*)
compile' (CDiv a b) = compileBinop' a b (/)
compile' (CAbs a) = do
  a' <- compile' a
  case a' of
    Pure pa -> return $ Pure (EAbs pa)
    Effectful ea -> Effectful <$> postprocess ea abs
compile' (CGT a b) = compileBinop' a b (%>)
compile' (CGE a b) = compileBinop' a b (%>=)
compile' (CLT a b) = compileBinop' a b (%<)
compile' (CLE a b) = compileBinop' a b (%<=)
compile' (CEQ a b) = compileBinop' a b (%==)
compile' (CNEQ a b) = compileBinop' a b (%/=)
-- these are not actually correct. we need to check what database the
-- continuation operated on. if it was the result of bmap, bfilter, bsum, etc,
-- then we can proceed. otherwise, we need to use something like the
-- `simdCombine` function above to keep temporary results in a tuple, and later
-- combine them all at once.
compile' (BMap (mf :: Expr (row -> row')) db kont) = do
  dbName <- checkDBName db
  lpush
  resultName <- lfresh "bmap_result"
  kontEffects <- compile' (kont (CVar resultName))
  lpop
  case kontEffects of
    Effectful (TypedEffects _ bound (kontMf :: Expr (shouldBeRow' -> _)) rf) ->
      case eqTypeRep (typeRep @row') (typeRep @shouldBeRow') of
        Just HRefl ->
          let newMf = kontMf `ecomp` mf
           in return . Effectful $ TypedEffects dbName bound newMf rf
        Nothing ->
          throwM $ TypeError (SomeTypeRep (typeRep @row')) (SomeTypeRep (typeRep @shouldBeRow'))
    Pure _ ->
      throwM $ InternalError "compile': impossible, the continuation of BMap has no side effects"
compile'
  ( BFilter
      (pred :: Expr (row -> Bool))
      db
      (kont :: (CPSFuzz (Bag row) -> CPSFuzz r))
    ) = do
    dbName <- checkDBName db
    lpush
    resultName <- lfresh "bfilter_result"
    kontEffects <- compile' (kont (CVar resultName))
    lpop
    case kontEffects of
      Effectful (TypedEffects _ bound
                  (kontMf :: Expr (shouldBeRow -> aggr))
                  (rf :: Expr (aggr -> Distr r))) ->
        case eqTypeRep (typeRep @row) (typeRep @shouldBeRow) of
          Just HRefl ->
            let pred' = (fromDeepRepr pred) :: (Expr row -> Expr Bool)
                rf'   = (fromDeepRepr rf) :: (Expr aggr -> Expr (Distr r))
                newMf row = eIf (pred' row) (eJust (kontMf %@ row)) eNothing
                newRf aggrEmpty maybeAggr =
                  eIf (eIsJust maybeAggr)
                    (rf' (eFromJust maybeAggr))
                    (rf' aggrEmpty)
                -- we need `VecMonoid aggr` here
                withAggrEmpty :: VecMonoid aggr => Expr aggr -> m (Compiled r)
                withAggrEmpty aggrEmpty = do
                  return . Effectful $
                    TypedEffects dbName bound
                      (toDeepRepr newMf)
                      (toDeepRepr $ newRf aggrEmpty)
             in ifCxt (Proxy :: Proxy (VecMonoid aggr))
                  (withAggrEmpty (eMonoidEmpty @aggr))
                  (throwM $ RequiresVecMonoid (SomeTypeRep (typeRep @aggr)))
          Nothing ->
            throwM $ TypeError (SomeTypeRep (typeRep @row)) (SomeTypeRep (typeRep @shouldBeRow))
      Pure _ ->
        throwM $ InternalError "compile': impossible, the continuation of BFilter has no side effects"
compile' (BSum clipBound db kont) = do
  dbName <- checkDBName db
  lpush
  resultName <- lfresh "bsum_result"
  kontEffects <- compile' (kont (CVar resultName))
  lpop
  case kontEffects of
    Effectful (TypedEffects _ bound kontMF kontRF) -> undefined
    Pure pa -> undefined

checkDBName :: MonadThrow m => CPSFuzz (Bag a) -> m String
checkDBName (CVar x) = return x
checkDBName _ =
  throwM . InternalError $ "checkDBName: expected db input term to be variable"

-- ##################
-- # INFRASTRUCTURE #
-- ##################

instance FreshM Compiler where
  getNameState = get
  modifyNameState f = modify f

instance Exception CompilerError
