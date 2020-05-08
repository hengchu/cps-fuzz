import Examples hiding ((>>=), return)
import Syntax
import HFunctor
import Names
import Data.Either
import Test.Hspec
import Control.Monad.State.Strict
import Type.Reflection
import Text.PrettyPrint.ANSI.Leijen
import Pretty

runNamed :: forall a. Typeable a => (forall f. HXFix CPSFuzzF f a) -> HFix NCPSFuzzF a
runNamed prog = fromRight undefined $
  flip evalStateT (nameState $ fvCPSFuzz prog) (named' prog)

runNamedFlatten :: forall a. Typeable a => (forall f. HXFix CPSFuzzF f a) -> HFix NNormalizedF a
runNamedFlatten prog = fromRight undefined $
  flip evalStateT (nameState $ fvCPSFuzz prog) (named' prog >>= flatten)

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
           (named' example5 >>= flatten))
      `shouldSatisfy` isRight

    it "runs without error: ex6" $ do
      (flip evalStateT
           (nameState $ fvCPSFuzz example6)
           (named' example6 >>= flatten))
      `shouldSatisfy` isRight

  describe "pNCPSFuzz" $ do
    it "prints example1 properly" $ example $ do
      putStrLn "=============================="
      putDoc (pNCPSFuzz $ runNamed example1)

    it "prints example2 properly" $ example $ do
      putStrLn "=============================="
      putDoc (pNCPSFuzz $ runNamed example2)

    it "prints example3 properly" $ example $ do
      putStrLn "=============================="
      putDoc (pNCPSFuzz $ runNamed example3)

    it "prints example4 properly" $ example $ do
      putStrLn "=============================="
      putDoc (pNCPSFuzz $ runNamed example4)

    it "prints example5 properly" $ example $ do
      putStrLn "=============================="
      putDoc (pNCPSFuzz $ runNamed example5)

    it "prints example6 properly" $ example $ do
      putStrLn "=============================="
      putDoc (pNCPSFuzz $ runNamed example6)

    it "prints example7 properly" $ example $ do
      putStrLn "=============================="
      putDoc (pNCPSFuzz $ runNamed example7)

  describe "pNNormalized" $ do
    it "prints example1 properly" $ example $ do
      putStrLn "=============================="
      putDoc (pNNormalized $ runNamedFlatten example1)

    it "prints example2 properly" $ example $ do
      putStrLn "=============================="
      putDoc (pNNormalized $ runNamedFlatten example2)

    it "prints example3 properly" $ example $ do
      putStrLn "=============================="
      putDoc (pNNormalized $ runNamedFlatten example3)

    it "prints example4 properly" $ example $ do
      putStrLn "=============================="
      putDoc (pNNormalized $ runNamedFlatten example4)

    it "prints example5 properly" $ example $ do
      putStrLn "=============================="
      putDoc (pNNormalized $ runNamedFlatten example5)

    it "prints example6 properly" $ example $ do
      putStrLn "=============================="
      putDoc (pNNormalized $ runNamedFlatten example6)

    it "prints example7 properly" $ example $ do
      putStrLn "=============================="
      putDoc (pNNormalized $ runNamedFlatten example7)
