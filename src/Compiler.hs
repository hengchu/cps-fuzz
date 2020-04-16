module Compiler where

import Control.Lens
import Control.Monad.State.Strict
import qualified Data.Map.Strict as M
import Lib
import Names
import Type.Reflection

-- | An allocation value of (a, b) represents the range in the client-side vector
--  representation that holds values of the corresponding row.
type Allocation = (Int, Int)

data CompilerState
  = CompilerState
      { _csNames :: NameState,
        _csReprSize :: Int,
        _csMapFunction :: Expr (Vec Number -> Vec Number),
        _csReleaseFunction :: Expr (Vec Number -> Number),
        _csAllocation :: M.Map String Allocation
      }

makeLensesWith abbreviatedFields ''CompilerState

emptyCompilerState :: CompilerState
emptyCompilerState =
  CompilerState emptyNameState 0 (ELam id) eVecSum M.empty

newtype Compiler a = Compiler {runCompiler_ :: State CompilerState a}
  deriving (Functor, Applicative, Monad, MonadState CompilerState) via (State CompilerState)

runCompiler :: Compiler a -> (a, CompilerState)
runCompiler = flip runState emptyCompilerState . runCompiler_

{-
newtype DBT m a = DBT { runDBT_ :: ReaderT String m a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader String
           ) via (ReaderT String m)
  deriving (MonadTrans) via (ReaderT String)

type DBCompiler = DBT Compiler

runDBT :: String -> DBT m a -> m a
runDBT dbName = flip runReaderT dbName . runDBT_
-}

-- TODO: we need to figure out how to map intermediate map structures to
-- representations in the vector.
--
-- Even with CPS, the continuation themselves can hold captured values from
-- outer scopes, and only use those later on.
--
-- So, for each variable, the compiler needs to keep track of a map from the var
-- name to the representation details of the variable.
--
-- In fact, let's treat the client-side vector as a appendable memory tape. Each
-- variable has a stable(?) location on this tape. i.e. x -> [start, end], where
-- vec[start] ... vec[end] holds the representation of this variable. We need to
-- make sure non-overlapping operations also do not overwrite each other's
-- representations on the client-side vector.
--
-- Let's also make sure we reserve space for variables as soon as they are
-- introduced, this should allow us to make the variable locations on the memory
-- tape stable over its lifetime?
--
-- 1. BMap:    I guess nothing changes?
-- 2. BFilter: Split the vector into 2 halfs?
-- 3. BSum: collapse into 1 dimension?
compile ::
  forall a m.
  ( CFT a,
    MonadState CompilerState m,
    FreshM m
  ) =>
  (CPSFuzz (Bag a) -> CPSFuzz Number) ->
  String ->
  m (BMCS Number)
compile prog dbName = do
  initializeRepresentation (typeRep @a) dbName
  compile' (prog (CVar dbName))
  repr <- gets (^. reprSize)
  f <- gets (^. mapFunction)
  r <- gets (^. releaseFunction)
  return $ Run repr f r

-- | Uses the `VecStorable` instance of `a` to initialize the representation
--  information of the initial input bag.
initializeRepresentation ::
  forall a m.
  ( CFT a,
    MonadState CompilerState m
  ) =>
  TypeRep a ->
  String ->
  m ()
initializeRepresentation _ dbName = do
  let size = vecSize @a
  modify $ \st -> st & reprSize .~ size
  modify $ \st -> st & allocation %~ M.insert dbName (0, size -1)

-- | The concret impl of compile. Returns Just (some BMCS value) if the
--  compilation unit is simple: i.e. only contains arithmetics, and do not touch
--  the database. Returns Nothing otherwise, and the fusion is accumulated in
--  compiler state.
compile' ::
  forall r m.
  ( CFT r,
    BT r,
    MonadState CompilerState m,
    FreshM m
  ) =>
  CPSFuzz r ->
  m ()
compile' (CVar x) = do
  (readStart, readEnd) <- fetchReadRange x
  case eqTypeRep (typeRep @r) (typeRep @Number) of
    Nothing ->
      error $
        "compile: I don't know how to support type "
          ++ (show $ typeRep @r)
          ++ " in arithmetics yet"
    Just HRefl -> do
      let newReleaseFun = eFromVecPF @Number `ecomp` eFocus (eInt readStart) (eInt readEnd)
      modify $ \st -> st & releaseFunction .~ newReleaseFun
compile' (CNumLit x) = do
  let newReleaseFun = eLam $ \_ -> eNum x
  modify $ \st -> st & releaseFunction .~ newReleaseFun
compile' (CAdd a b) = do
  compile' a
  aReleaseFun <- gets (^. releaseFunction)
  compile' b
  bReleaseFun <- gets (^. releaseFunction)
  let newReleaseFun =
        eLam $ \vecRepr -> (aReleaseFun `eApp` vecRepr + bReleaseFun `eApp` vecRepr)
  modify $ \st -> st & releaseFunction .~ newReleaseFun
compile' (CMinus a b) = do
  compile' a
  aReleaseFun <- gets (^. releaseFunction)
  compile' b
  bReleaseFun <- gets (^. releaseFunction)
  let newReleaseFun =
        eLam $ \vecRepr -> (aReleaseFun `eApp` vecRepr - bReleaseFun `eApp` vecRepr)
  modify $ \st -> st & releaseFunction .~ newReleaseFun
compile' (CMult a b) = do
  compile' a
  aReleaseFun <- gets (^. releaseFunction)
  compile' b
  bReleaseFun <- gets (^. releaseFunction)
  let newReleaseFun =
        eLam $ \vecRepr -> (aReleaseFun `eApp` vecRepr * bReleaseFun `eApp` vecRepr)
  modify $ \st -> st & releaseFunction .~ newReleaseFun
compile' (CDiv a b) = do
  compile' a
  aReleaseFun <- gets (^. releaseFunction)
  compile' b
  bReleaseFun <- gets (^. releaseFunction)
  let newReleaseFun =
        eLam $ \vecRepr -> (aReleaseFun `eApp` vecRepr / bReleaseFun `eApp` vecRepr)
  modify $ \st -> st & releaseFunction .~ newReleaseFun
compile' (CAbs a) = do
  compile' a
  aReleaseFun <- gets (^. releaseFunction)
  let newReleaseFun =
        eLam $ \vecRepr -> (abs $ aReleaseFun `eApp` vecRepr)
  modify $ \st -> st & releaseFunction .~ newReleaseFun
compile' (CGT a b) = do
  compile' a
  aReleaseFun <- gets (^. releaseFunction)
  compile' b
  bReleaseFun <- gets (^. releaseFunction)
  let newReleaseFun =
        eLam $ \vecRepr -> (eB2N (aReleaseFun `eApp` vecRepr %> bReleaseFun `eApp` vecRepr))
  modify $ \st -> st & releaseFunction .~ newReleaseFun
compile' (CGE a b) = do
  compile' a
  aReleaseFun <- gets (^. releaseFunction)
  compile' b
  bReleaseFun <- gets (^. releaseFunction)
  let newReleaseFun =
        eLam $ \vecRepr -> (eB2N (aReleaseFun `eApp` vecRepr %>= bReleaseFun `eApp` vecRepr))
  modify $ \st -> st & releaseFunction .~ newReleaseFun
compile' (CLT a b) = do
  compile' a
  aReleaseFun <- gets (^. releaseFunction)
  compile' b
  bReleaseFun <- gets (^. releaseFunction)
  let newReleaseFun =
        eLam $ \vecRepr -> (eB2N (aReleaseFun `eApp` vecRepr %< bReleaseFun `eApp` vecRepr))
  modify $ \st -> st & releaseFunction .~ newReleaseFun
compile' (CLE a b) = do
  compile' a
  aReleaseFun <- gets (^. releaseFunction)
  compile' b
  bReleaseFun <- gets (^. releaseFunction)
  let newReleaseFun =
        eLam $ \vecRepr -> (eB2N (aReleaseFun `eApp` vecRepr %<= bReleaseFun `eApp` vecRepr))
  modify $ \st -> st & releaseFunction .~ newReleaseFun
compile' (CEQ a b) = do
  compile' a
  aReleaseFun <- gets (^. releaseFunction)
  compile' b
  bReleaseFun <- gets (^. releaseFunction)
  let newReleaseFun =
        eLam $ \vecRepr -> (eB2N (aReleaseFun `eApp` vecRepr %== bReleaseFun `eApp` vecRepr))
  modify $ \st -> st & releaseFunction .~ newReleaseFun
compile' (CNEQ a b) = do
  compile' a
  aReleaseFun <- gets (^. releaseFunction)
  compile' b
  bReleaseFun <- gets (^. releaseFunction)
  let newReleaseFun =
        eLam $ \vecRepr -> (eB2N (aReleaseFun `eApp` vecRepr %/= bReleaseFun `eApp` vecRepr))
  modify $ \st -> st & releaseFunction .~ newReleaseFun
compile' (BMap (f :: Expr (a -> b)) db k) = do
  dbName <- checkDBName db
  (readStart, readEnd) <- fetchReadRange dbName
  bmapOutDbName <- gfresh $ dbName ++ "_bmap_output"
  (writeStart, writeEnd) <- allocate (typeRep @b) bmapOutDbName
  let newMapFunction =
        eVecStore
          (eInt readStart)
          (eInt readEnd)
          (eInt writeStart)
          (eInt writeEnd)
          (eAsVecPF `ecomp` f `ecomp` eFromVecPF)
  modify $ \st -> st & mapFunction %~ (newMapFunction `ecomp`)
  compile' (k (CVar bmapOutDbName))
compile' (BFilter (f :: Expr (a -> Bool)) db k) = do
  dbName <- checkDBName db
  (readStart, readEnd) <- fetchReadRange dbName
  bfilterOutDbName <- gfresh $ dbName ++ "_bfilter_output"
  (writeStart, writeEnd) <- allocate (typeRep @a) bfilterOutDbName
  let newMapFunction =
        eVecStore
          (eInt readStart)
          (eInt readEnd)
          (eInt writeStart)
          (eInt writeEnd)
          f'
  modify $ \st -> st & mapFunction %~ (newMapFunction `ecomp`)
  compile' (k (CVar bfilterOutDbName))
  where
    f' = eLam $ \vecRepr ->
      eIf (f `eApp` (eFromVec vecRepr)) vecRepr (eVecZeros (eInt (vecSize @a)))
compile' (BSum clipBound db k) = do
  dbName <- checkDBName db
  (readStart, readEnd) <- fetchReadRange dbName
  bsumOutName <- gfresh $ dbName ++ "_bsum_output"
  (writeStart, writeEnd) <- allocate (typeRep @Number) bsumOutName
  let newMapFunction =
        eVecStore
          (eInt readStart)
          (eInt readEnd)
          (eInt writeStart)
          (eInt writeEnd)
          f'
  modify $ \st -> st & mapFunction %~ (newMapFunction `ecomp`)
  compile' (k (CVar bsumOutName))
  where
    f' = eAsVecPF `ecomp` eClip (eNum clipBound) `ecomp` (eFromVecPF @Number)
    eClip bound = eLam $ \arg ->
      eIf
        (arg %> bound)
        bound
        ( eIf
            (arg %< - bound)
            (- bound)
            arg
        )

fetchReadRange :: MonadState CompilerState m => String -> m (Int, Int)
fetchReadRange dbName = do
  allocMap <- gets (^. allocation)
  case M.lookup dbName allocMap of
    Nothing -> error $ "compile: db " ++ dbName ++ " has no associated range"
    Just range -> return range

checkDBName :: Monad m => CPSFuzz (Bag a) -> m String
checkDBName db =
  case db of
    CVar dbName -> return dbName
    _ -> error $ "compile: db argument is not a variable?!"

-- | Takes a `VecStorable` type, the associated `dbName`, and allocates on the
--  client-side vector enough space to store each row of the new db.
allocate ::
  forall b m.
  (CFT b, MonadState CompilerState m) =>
  TypeRep b ->
  String ->
  m (Int, Int)
allocate _ dbName = do
  currReprSize <- gets (^. reprSize)
  let bVecSize = vecSize @b
  let writeStart = currReprSize
  let writeEnd = writeStart + bVecSize - 1
  modify $ \st -> st & reprSize %~ (+ bVecSize)
  modify $ \st -> st & allocation %~ (M.insert dbName (writeStart, writeEnd))
  modify $ \st -> st & mapFunction %~ (eVecExtend (eInt bVecSize) `ecomp`)
  return (writeStart, writeEnd)

-- ##################
-- # INFRASTRUCTURE #
-- ##################

instance FreshM Compiler where
  getNameState = gets (^. names)
  modifyNameState f = modify (\st -> st & names %~ f)
