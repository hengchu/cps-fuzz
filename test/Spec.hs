import Examples hiding ((>>=), return)
import Syntax
import HFunctor
import Names
import Data.Either
import Test.Hspec
import Control.Monad.State.Strict

main :: IO ()
main = hspec $ do
  describe "openM" $ do
    it "runs without error" $ do
      fmap fst
        (flip evalStateT
           (nameState $ fvCPSFuzz example3)
           (named' example3 >>= openM))
      `shouldSatisfy` isRight

    it "returns correctly bound names" $ do
      fromRight undefined $ fmap fst
        (flip evalStateT
           (nameState $ fvCPSFuzz example3)
           (named' example3 >>= openM))
      `shouldBe` (Var "foo")

  describe "flatten" $ do
    it "runs without error: ex3" $ do
      (flip evalStateT
           (nameState $ fvCPSFuzz example3)
           (named' example3 >>= flatten))
      `shouldSatisfy` isRight

    it "runs without error: ex5" $ do
      (flip evalStateT
           (nameState $ fvCPSFuzz example5)
           (named' example3 >>= flatten))
      `shouldSatisfy` isRight

    it "runs without error: ex6" $ do
      (flip evalStateT
           (nameState $ fvCPSFuzz example6)
           (named' example3 >>= flatten))
      `shouldSatisfy` isRight
