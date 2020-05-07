module Compiler where

import Control.Lens
import Control.Monad.Catch
import qualified Data.Map.Strict as M
import HFunctor
import Syntax
import Type.Reflection

data Edge from to where
  Map :: HFix NRedZoneF (from -> to) -> Edge (Bag from) (Bag to)
  Sum :: Vec Number -> Edge (Bag sum) sum

data AnyEdge where
  AnyEdge :: (Typeable from, Typeable to) => Edge from to -> AnyEdge

type Direction = (String, String)

data EffectGraph
  = EG
      { _egEdges :: M.Map Direction AnyEdge,
        _egNeighbors :: M.Map String [String],
        _egParents :: M.Map String String,
        _egTypes :: M.Map String SomeTypeRep
      }

makeLensesWith abbreviatedFields ''EffectGraph

emptyEG :: EffectGraph
emptyEG = EG M.empty M.empty M.empty M.empty

data Effect r
  = Eff
      { _eGraph :: EffectGraph,
        _eNormalized :: HFix NNormalizedF r
      }

makeLensesWith abbreviatedFields ''Effect

data CompilerError
  = InternalError String
  deriving (Show)

instance Exception CompilerError

unpack :: Effect a -> ((K EffectGraph) :* (HFix NNormalizedF)) a
unpack (Eff a b) = Prod (K a) b

combine :: MonadThrowWithStack m => K EffectGraph a -> HFix NNormalizedF a -> m (Effect a)
combine (unK -> g) term = return $ Eff g term

traceMCSEffectM ::
  ( MonadThrowWithStack m,
    HInject FlatBagOpF h
  ) =>
  h (K EffectGraph) a ->
  m (K EffectGraph a)
traceMCSEffectM = undefined

effects ::
  MonadThrow m =>
  HFix NNormalizedF a ->
  m (Effect a)
effects =
  hcataM' (prodAlgWithM traceMCSEffectM (return . wrap) combine . hmap unpack)
