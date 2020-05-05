module Pretty where

import Control.Monad.State.Strict
import Names
import Text.PrettyPrint.ANSI.Leijen
import Type.Reflection

-- | The pretty-printing monad.
newtype P a = P {runP_ :: State NameState a}
  deriving
    (Functor, Applicative, Monad, MonadState NameState)
    via (State NameState)

associativityTable :: String -> Int
associativityTable "App" = 1
associativityTable "." = -1
associativityTable "+" = 1
associativityTable "-" = 1
associativityTable "*" = 1
associativityTable "/" = 1
associativityTable "&&" = -1
associativityTable "||" = -1
associativityTable "==" = 0
associativityTable "/=" = 0
associativityTable "<" = 0
associativityTable "<=" = 0
associativityTable ">" = 0
associativityTable ">=" = 0
associativityTable x = error $ "associativityTable: unknown symbol " ++ x

precedenceTable :: String -> Int
precedenceTable "App" = 900
precedenceTable "." = 800
precedenceTable "*" = 700
precedenceTable "/" = 700
precedenceTable "+" = 600
precedenceTable "-" = 600
precedenceTable "==" = 400
precedenceTable "/=" = 400
precedenceTable "<" = 400
precedenceTable "<=" = 400
precedenceTable ">" = 400
precedenceTable ">=" = 400
precedenceTable "&&" = 300
precedenceTable "||" = 200
precedenceTable x = error $ "precedenceTable: unknown symbol " ++ x

runP :: P a -> a
runP = flip evalState emptyNameState . runP_
