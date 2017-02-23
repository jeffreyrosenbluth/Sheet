{-# LANGUAGE ViewPatterns #-}

module Main where

import           Lib

import           Control.Monad.State.Strict
import qualified Data.ByteString as BS
import           Data.Serialize (encode, decode)
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

save :: Model -> IO ()
save model = BS.writeFile fname bs
  where
    fname = file model
    bs = encode $ sheet model

repl :: Repl ()
repl = do
  minput <- getInputLine ">> "
  case minput of
    Nothing -> outputStrLn "Goodbye."
    Just (words -> "new":fname:[]) -> do
      lift $ modify (\m -> m {file = fname})
      repl
    Just (words -> "open":fname:[]) -> do
      eFile <- liftIO $ BS.readFile fname
      case decode eFile of
        Left err -> outputStrLn $ "*** CANNOT DECODE FILE: " ++ err ++ " ***"
        Right events -> lift $ modify (\m -> m { sheet = events, file = fname})
      repl
    Just "quit" -> outputStrLn "Goodbye"
    Just "show" -> do
      m <- lift get
      outputStrLn . displaySheet . sheet $ m
      repl
    Just "total" -> do
      m <- lift get
      outputStrLn $ (displayEntry . total) (sheet m)
      repl
    Just ('+':' ':eventString) -> do
      case readP_to_S parseEvent eventString of
        [] -> do
          outputStrLn "*** INVALID EVENT ***"
          repl
        ((e, _):_) -> do
          lift $ modify
            (\m -> m { sheet = e:sheet m , nextId = nextId m + 1})
          m' <-  lift get
          liftIO $ save m'
          repl
    Just input -> do
      outputStrLn $ "*** " ++ input ++ " IS NOT A VALID COMMAND ***"
      repl

main :: IO ((), Model)
main = runStateT (runInputT defaultSettings repl) emptyModel
