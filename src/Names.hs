module Names where

import Lib
import qualified Data.Map.Strict as M
import Control.Lens
import Data.Semigroup

type NameMap = M.Map String Int

data NameState = NameState {
  _nsGlobals  :: NameMap     -- ^the currently in-scope global names
  , _nsLocals :: [NameMap]   -- ^the currently in-scope local names
  } deriving (Show, Eq, Ord)

makeLensesWith abbreviatedFields ''NameState

class Monad m => FreshM m where
  getNameState :: m NameState
  modifyNameState :: (NameState -> NameState) -> m ()

  -- |Get a globally fresh name.
  gfresh :: String -> m String
  gfresh hint = do
    ns <- getNameState
    let gnames = ns ^. globals
    let lcxts  = ns ^. locals
    let allCxts = gnames:lcxts
    let nextNameId = foldMap ((fmap Max) . M.lookup hint) allCxts
    case fmap getMax nextNameId of
      Nothing -> do
        modifyNameState (\st -> st & globals %~ M.insert hint 1)
        modifyNameState (\st -> st & locals  %~ map (M.insert hint 1))
        return hint
      Just nextIdx -> do
        modifyNameState (\st -> st & globals %~ M.insert hint (nextIdx + 1))
        modifyNameState (\st -> st & locals  %~ map (M.insert hint (nextIdx + 1)))
        return (hint ++ show nextIdx)

  -- |Enter a new locally fresh context.
  lpush :: m ()
  lpush = do
    ns <- getNameState
    let gnames = ns ^. globals
    let lcxts  = ns ^. locals
    case lcxts of
      []     -> modifyNameState (\st -> st & locals %~ (gnames:))
      (c:_)  -> modifyNameState (\st -> st & locals %~ (c:))

  -- |Exit the last locally fresh context.
  lpop :: m ()
  lpop = do
    ns <- getNameState
    let lcxts = ns ^. locals
    case lcxts of
      []     -> error "lpop: no pushed contexts!"
      (_:cs) -> modifyNameState (\st -> st & locals %~ (const cs))

  -- |Get a locally fresh name.
  lfresh :: String -> m String
  lfresh hint = do
    ns <- getNameState
    let lcxts = ns ^. locals
    case lcxts of
      [] -> error "lfresh: no pushed contexts!"
      (c:cs) -> case M.lookup hint c of
                  Nothing -> do
                    let c' = M.insert hint 1 c
                    modifyNameState (\st -> st & locals %~ (const $ c':cs))
                    return hint
                  Just nextIdx -> do
                    let c' = M.insert hint (nextIdx + 1) c
                    modifyNameState (\st -> st & locals %~ (const $ c':cs))
                    return (hint ++ show nextIdx)


emptyNameState :: NameState
emptyNameState = NameState (M.fromList [(secretVarName, 1)]) []
