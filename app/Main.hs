module Main where

import           Lib

import           Control.Monad.State.Strict
import           System.Console.Haskeline
import           Text.ParserCombinators.ReadP (readP_to_S)

data Model = Model
  { nextId :: Int
  , sheet  :: Sheet
  , file   :: String
  }

emptyModel :: Model
emptyModel = Model 0 [] ""

type Repl a = InputT (StateT Model IO) a

process :: String -> IO ()
process = putStrLn

repl :: Repl ()
repl = do
  minput <- getInputLine ">> "
  case minput of
    Nothing -> outputStrLn "Goodbye."
    Just "quit" -> outputStrLn "Goodbye"
    Just "show" -> do
      m <- lift get
      outputStrLn . show . sheet $ m
      repl
    Just ('+' : ' ' : eventString) -> do
      case (readP_to_S parseEvent eventString) of
        [] -> repl
        ((e, _):_) -> lift $ modify (\m -> m {sheet = e:(sheet m)})
      repl
    Just input -> (liftIO $ process input) >> repl

main :: IO ((), Model)
main = runStateT (runInputT defaultSettings repl) emptyModel
