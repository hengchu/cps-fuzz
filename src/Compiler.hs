module Compiler where

import Lib

import Control.Lens
import Control.Monad.State.Strict
import Control.Monad.Reader
import qualified Data.Map.Strict as M
import Type.Reflection

type NameMap = M.Map String Int

data NameState = NameState {
  _nsGlobals  :: NameMap     -- ^the currently in-scope global names
  , _nsLocals :: [NameMap]   -- ^the currently in-scope local names
  } deriving (Show, Eq, Ord)

makeLensesWith abbreviatedFields ''NameState

-- |An allocation value of (a, b) represents the range in the client-side vector
-- representation that holds values of the corresponding row.
type Allocation = (Int, Int)

data CompilerState = CompilerState {
  _csNames :: NameState
  , _csClipBound :: Number
  , _csReprSize :: Int
  , _csMapFunction :: Expr (Vec Number -> Vec Number)
  , _csReleaseFunction :: Expr (Vec Number -> Number)
  , _csAllocation :: M.Map String Allocation
  }

makeLensesWith abbreviatedFields ''CompilerState

class Monad m => FreshM m where
  -- |Get a globally fresh name.
  gfresh :: String -> m String

  -- |Enter a new locally fresh context.
  lpush :: m ()

  -- |Exit the last locally fresh context.
  lpop :: m ()

  -- |Get a locally fresh name.
  lfresh :: String -> m String

emptyNameState :: NameState
emptyNameState = NameState (M.fromList [(secretVarName, 1)]) []

emptyCompilerState :: CompilerState
emptyCompilerState =
  CompilerState emptyNameState 0 0 (ELam id) eVecSum M.empty

newtype Compiler a = Compiler { runCompiler_ :: State CompilerState a }
  deriving (Functor, Applicative, Monad, MonadState CompilerState) via (State CompilerState)

runCompiler :: Compiler a -> (a, CompilerState)
runCompiler = flip runState emptyCompilerState . runCompiler_

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
compile :: forall a r m .
           ( CFT a
           , CFT r
           , BT r
           , MonadReader String m
           , MonadState CompilerState m
           , FreshM m
           )
        => (CPSFuzz (Bag a) -> CPSFuzz r)
        -> m (BMCS r)
compile prog = do
  initializeRepresentation (typeRep @a)
  undefined

-- |Uses the `VecStorable` instance of `a` to initialize the representation
-- information of the initial input bag.
initializeRepresentation :: forall a m.
                            ( CFT a
                            , MonadReader String m
                            , MonadState CompilerState m)
                         => TypeRep a -> m ()
initializeRepresentation _ = do
  let size = vecSize @a
  dbName <- ask
  modify $ \st -> st & reprSize .~ size
  modify $ \st -> st & allocation %~ M.insert dbName (0, size-1)

-- |The concret impl of compile. Returns Just (some BMCS value) if the
-- compilation unit is simple: i.e. only contains arithmetics, and do not touch
-- the database. Returns Nothing otherwise, and the fusion is accumulated in
-- compiler state.
compile' :: ( CFT r
            , BT r
            , MonadReader String m
            , MonadState CompilerState m
            , FreshM m
            )
         => CPSFuzz r
         -> m (Maybe (BMCS r))
compile' (CVar x) = do
  dbName <- ask
  if x == dbName
  then return Nothing
  else return (Just (BVar x))
compile' (CNumLit x) = do
  return $ Just (BNumLit x)
compile' (CAdd a b) = do
  a' <- compile' a
  b' <- compile' b
  case (a', b') of
    (Just a', Just b') -> return $ Just (BAdd a' b')
    _ -> error "compile: arithmetic contains complex operations"
compile' (CMinus a b) = do
  a' <- compile' a
  b' <- compile' b
  case (a', b') of
    (Just a', Just b') -> return $ Just (BMinus a' b')
    _ -> error "compile: arithmetic contains complex operations"
compile' (CMult a b) = do
  a' <- compile' a
  b' <- compile' b
  case (a', b') of
    (Just a', Just b') -> return $ Just (BMult a' b')
    _ -> error "compile: arithmetic contains complex operations"
compile' (CDiv a b) = do
  a' <- compile' a
  b' <- compile' b
  case (a', b') of
    (Just a', Just b') -> return $ Just (BDiv a' b')
    _ -> error "compile: arithmetic contains complex operations"
compile' (CAbs x) = do
  x' <- compile' x
  case x' of
    Just x' -> return $ Just (BAbs x')
    _ -> error "compile: abs contains complex operations"
compile' (CGT a b) = do
  a' <- compile' a
  b' <- compile' b
  case (a', b') of
    (Just a', Just b') -> return $ Just (BGT a' b')
    _ -> error "compile: comparison contains complex operations"
compile' (CGE a b) = do
  a' <- compile' a
  b' <- compile' b
  case (a', b') of
    (Just a', Just b') -> return $ Just (BGE a' b')
    _ -> error "compile: comparison contains complex operations"
compile' (CLT a b) = do
  a' <- compile' a
  b' <- compile' b
  case (a', b') of
    (Just a', Just b') -> return $ Just (BLT a' b')
    _ -> error "compile: comparison contains complex operations"
compile' (CLE a b) = do
  a' <- compile' a
  b' <- compile' b
  case (a', b') of
    (Just a', Just b') -> return $ Just (BLE a' b')
    _ -> error "compile: comparison contains complex operations"
compile' (CEQ a b) = do
  a' <- compile' a
  b' <- compile' b
  case (a', b') of
    (Just a', Just b') -> return $ Just (BEQ a' b')
    _ -> error "compile: comparison contains complex operations"
compile' (CNEQ a b) = do
  a' <- compile' a
  b' <- compile' b
  case (a', b') of
    (Just a', Just b') -> return $ Just (BNEQ a' b')
    _ -> error "compile: comparison contains complex operations"
compile' (BMap (f :: Expr (a -> b)) db k) = do
  dbName <- ask
  checkDBName db dbName
  let bVecSize = vecSize @b
  allocMap <- gets (^. allocation)
  (readStart, readEnd) <- fetchReadRange dbName
  bmapOutDbName <- gfresh $ dbName ++ "_bmap_output"
  (writeStart, writeEnd) <- allocate (typeRep @a) bmapOutDbName
  let newMapFunction =
        eVecStore
          (eInt readStart) (eInt readEnd)
          (eInt writeStart) (eInt writeEnd)
          (eAsVecPF `ecomp` f `ecomp` eFromVecPF)
  modify $ \st -> st & mapFunction .~ newMapFunction
  local (const bmapOutDbName) $ compile' (k (CVar bmapOutDbName))
compile' (BFilter (f :: Expr (a -> Bool)) db k) = do
  dbName <- ask
  checkDBName db dbName
  let aVecSize = vecSize @a
  (readStart, readEnd) <- fetchReadRange dbName
  bfilterOutDbName <- gfresh $ dbName ++ "_bfilter_output"
  (writeStart, writeEnd) <- allocate (typeRep @a) bfilterOutDbName
  let newMapFunction =
        eVecStore
          (eInt readStart) (eInt readEnd)
          (eInt writeStart) (eInt writeEnd)
          undefined
  undefined

fetchReadRange :: MonadState CompilerState m => String -> m (Int, Int)
fetchReadRange dbName = do
  allocMap <- gets (^. allocation)
  case M.lookup dbName allocMap of
    Nothing -> error $ "compile: db " ++ dbName ++ " has no associated range"
    Just range -> return range

checkDBName :: Monad m => CPSFuzz (Bag a) -> String -> m ()
checkDBName db dbName =
  case db of
    CVar dbName' ->
      if dbName' /= dbName
      then error $ "compile: db argument is " ++ show dbName' ++ ", but we expected " ++ show dbName
      else return ()
    _ -> error $ "compile: db argument is not a variable?!"

-- |Takes a `VecStorable` type, the associated `dbName`, and allocates on the
-- client-side vector enough space to store each row of the new db.
allocate :: forall b m.
            (CFT b, MonadState CompilerState m)
         => TypeRep b
         -> String
         -> m (Int, Int)
allocate _ dbName = do
  currReprSize <- gets (^. reprSize)
  let bVecSize = vecSize @b
  let writeStart = currReprSize
  let writeEnd = writeStart + bVecSize - 1
  modify $ \st -> st & reprSize %~ (+bVecSize)
  modify $ \st -> st & allocation %~ (M.insert dbName (writeStart, writeEnd))
  modify $ \st -> st & mapFunction %~ (eVecExtend (eInt bVecSize) `ecomp`)
  return (writeStart, writeEnd)

-- ##################
-- # INFRASTRUCTURE #
-- ##################

instance FreshM Compiler where
  gfresh hint = do
    gnames <- gets (^. names . globals)
    case M.lookup hint gnames of
      Nothing -> do
        modify (\st -> st & names . globals %~ M.insert hint 1)
        return hint
      Just nextIdx -> do
        modify (\st -> st & names . globals %~ M.insert hint (nextIdx + 1))
        return (hint ++ show nextIdx)
  lpush = do
    gnames <- gets (^. names . globals)
    lcxts  <- gets (^. names . locals)
    case lcxts of
      []     -> modify (\st -> st & names . locals %~ (gnames:))
      (c:cs) -> modify (\st -> st & names . locals %~ (c:))
  lpop = do
    lcxts <- gets (^. names . locals)
    case lcxts of
      []     -> error "lpop: no pushed contexts!"
      (c:cs) -> modify (\st -> st & names . locals %~ (const cs))
  lfresh hint = do
    lcxts <- gets (^. names . locals)
    case lcxts of
      [] -> error "lfresh: no pushed contexts!"
      (c:cs) -> case M.lookup hint c of
                  Nothing -> do
                    let c' = M.insert hint 1 c
                    modify (\st -> st & names . locals %~ (const $ c':cs))
                    return hint
                  Just nextIdx -> do
                    let c' = M.insert hint (nextIdx + 1) c
                    modify (\st -> st & names . locals %~ (const $ c':cs))
                    return (hint ++ show nextIdx)
