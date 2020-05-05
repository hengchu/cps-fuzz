{-# LANGUAGE RebindableSyntax #-}

module Examples where

import Data.String
import Prelude
import Syntax

-- ############
-- # EXAMPLES #
-- ############

example1 :: forall f. CPSFuzz f (Int -> Int)
example1 = toDeepRepr $ \(N x :: Name "abc" (CPSFuzz f Int)) -> x

example2 :: forall f. CPSFuzz f Bool
example2 = xwrap . hinject' $ (XEVarF "y")

-- Note that this "y" does not capture the "y" inside. It will get automatically
-- alpha-converted.
example3 :: forall f. CPSFuzz f (Int -> Bool)
example3 = toDeepRepr $ \(N _ :: (Name "foo" (CPSFuzz f Int))) -> (example2 @f)

example4 :: forall f. CPSFuzz f (Number -> Distr Number)
example4 = toDeepRepr $ \(N w :: Name "w" (CPSFuzz f Number)) -> do
  $(named "noised") <- lap 1.0 w
  return noised
  -- Some funny syntax tricks here to allow us name things inside a monad.
  where (>>=) = (>>=.)
        return = ret
