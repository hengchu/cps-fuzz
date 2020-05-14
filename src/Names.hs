module Names where

import Control.Lens
import Control.Monad.State.Strict
import Data.Foldable
import qualified Data.Map.Strict as M
import Data.Semigroup
import Text.Printf
import Data.Hashable
import GHC.Generics

type NameMap = M.Map String Int

data UniqueName
  = UniqueName
      { _nBase :: String,
        _nTrailing :: Int
      }
  deriving (Eq, Ord, Generic)

instance Hashable UniqueName

instance Show UniqueName where
  show (UniqueName b t) = printf "%s_%d_" b t

data NameState
  = NameState
      { -- | the currently in-scope global names
        _nsGlobals :: NameMap,
        -- | the currently in-scope local names
        _nsLocals :: [NameMap]
      }
  deriving (Show, Eq, Ord)

makeLensesWith abbreviatedFields ''NameState

gfreshAppend :: FreshM m => UniqueName -> String -> m UniqueName
gfreshAppend (UniqueName b idx) postfix =
  gfresh $ b ++ postfix

class Monad m => FreshM m where
  getNameState :: m NameState
  modifyNameState :: (NameState -> NameState) -> m ()

  -- | Get a globally fresh name.
  gfresh :: String -> m UniqueName
  gfresh hint = do
    ns <- getNameState
    let gnames = ns ^. globals
    let lcxts = ns ^. locals
    let allCxts = gnames : lcxts
    let nextNameId = foldMap ((fmap Max) . M.lookup hint) allCxts
    case fmap getMax nextNameId of
      Nothing -> do
        modifyNameState (\st -> st & globals %~ M.insert hint 1)
        modifyNameState (\st -> st & locals %~ map (M.insert hint 1))
        return $ UniqueName hint 0
      Just nextIdx -> do
        modifyNameState (\st -> st & globals %~ M.insert hint (nextIdx + 1))
        modifyNameState (\st -> st & locals %~ map (M.insert hint (nextIdx + 1)))
        return $ UniqueName hint nextIdx

  -- | Enter a new locally fresh context.
  lpush :: m ()
  lpush = do
    ns <- getNameState
    let gnames = ns ^. globals
    let lcxts = ns ^. locals
    case lcxts of
      [] -> modifyNameState (\st -> st & locals %~ (gnames :))
      (c : _) -> modifyNameState (\st -> st & locals %~ (c :))

  -- | Exit the last locally fresh context.
  lpop :: m ()
  lpop = do
    ns <- getNameState
    let lcxts = ns ^. locals
    case lcxts of
      [] -> error "lpop: no pushed contexts!"
      (_ : cs) -> modifyNameState (\st -> st & locals %~ (const cs))

  -- | Get a locally fresh name.
  lfresh :: String -> m UniqueName
  lfresh hint = do
    ns <- getNameState
    let lcxts = ns ^. locals
    case lcxts of
      [] -> error "lfresh: no pushed contexts!"
      (c : cs) -> case M.lookup hint c of
        Nothing -> do
          let c' = M.insert hint 1 c
          modifyNameState (\st -> st & locals %~ (const $ c' : cs))
          return $ UniqueName hint 0
        Just nextIdx -> do
          let c' = M.insert hint (nextIdx + 1) c
          modifyNameState (\st -> st & locals %~ (const $ c' : cs))
          return $ UniqueName hint nextIdx

instance Monad m => FreshM (StateT NameState m) where
  getNameState = get
  modifyNameState = modify

emptyNameState :: NameState
emptyNameState = NameState M.empty []

nameState :: Foldable t => t UniqueName -> NameState
nameState inScope = NameState globals []
  where
    globals = foldr (\(UniqueName name idx) -> M.insertWith max name idx) M.empty inScope
