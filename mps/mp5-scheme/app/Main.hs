module Main where
import Scheme.Runtime
import Scheme.Core
import Scheme.Parse
import Scheme.Eval

import Prelude hiding (lookup)
import System.IO (hFlush, hPutStr, hPutStrLn, hGetLine, stdin, stdout)
import Text.ParserCombinators.Parsec hiding (Parser, State)
import Control.Monad
import Control.Monad.State
import Control.Monad.Except

--- ### REPL

printLn :: String -> IO ()
printLn str = hPutStrLn stdout str >> hFlush stdout

repl :: Env -> IO ()
repl env = do
  putStr "scheme> "
  hFlush stdout
  l <- getLine                                        -- Read
  case parse exprP "Expression" l of                  -- Parse
    Left err -> print err                             -- Diagnostics
    Right expr ->
        -- runExcept returns a value of type `Either Diagnostic (Val, Env)`
      case runExcept $ runStateT (eval expr) env of   -- Eval
        Left err -> print err
        -- TODO:
        Right (Void, newEnv) -> repl newEnv           -- If return value is void, loop with new env without printing
        Right (val, newEnv) -> do                     -- Insert line here: Otherwise, print and loop with new env
          print val
          repl newEnv

main :: IO ()
main = repl runtime
