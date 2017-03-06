{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -fno-warn-orphans  #-}

module Main where

import           Sheet

import           Control.Monad.State.Strict
import qualified Data.ByteString as BS
import           Data.Serialize (encode, decode)
import           System.Console.Haskeline
import           System.Directory (doesFileExist)
import           Text.ParserCombinators.ReadP (readP_to_S)
import           Text.Read (readMaybe)

main :: IO ((), Model)
main = runStateT (runInputT defaultSettings repl) emptyModel

data Model = Model
  { nextId :: Int
  , sheet  :: Sheet
  , file   :: String
  }

emptyModel :: Model
emptyModel = Model 0 [] ""

instance MonadState s m => MonadState s (InputT m) where
  get = lift get
  put = lift . put
  state = lift . state

type Repl a = InputT (StateT Model IO) a

save :: Repl ()
save =  do
  m <- get
  liftIO $ BS.writeFile (file m) (encode $ sheet m)

repl :: Repl ()
repl = do
  command <- getInputLine ">> "
  case command of
    Nothing -> outputStrLn "Goodbye."
    Just (words -> "new" : fname : []) -> do
      modify (\m -> m {file = fname})
      repl
    Just (words -> "open" : fname : []) -> do
      exists <- liftIO $ doesFileExist fname
      if exists
        then do
          efile <- liftIO $ BS.readFile fname
          case decode efile of
            Left err -> outputStrLn $ "*** CANNOT DECODE FILE: " ++ err ++ " ***"
            Right events ->
              modify
                (\m ->
                  m { nextId = (length events)
                    , sheet = events
                    , file = fname
                    }
                )
        else
          outputStrLn $ "*** FILE: " ++ fname ++ " DOES NOT EXIST. ***"
      repl
    Just "quit" -> outputStrLn "goodbye"
    Just "show" -> do
      m <- get
      outputStrLn . displaySheet . sheet $ m
      repl
    Just "total" -> do
      m <- get
      outputStrLn $ (displayEntry . total) (sheet m)
      repl
    Just ('+' : ' ' : eventstring) ->
      case readP_to_S parseEvent eventstring of
        [] -> do
          outputStrLn "*** INVALID EVENT ***"
          repl
        ((e, _):_) -> do
          modify
            (\m ->
               m { sheet = (e {ident = nextId m + 1}):sheet m
                 , nextId = nextId m + 1
                 }
            )
          save
          repl
    Just (words -> "delete" : n : []) ->
      case readMaybe n of
        Nothing -> do
          outputStrLn $ "*** EVENT " ++ n ++ " DOES NOT EXIST ***"
          repl
        Just n' -> do
          modify (\m -> m {sheet = deleteEntry (sheet m) n'})
          save
          repl
    Just "reconcile" -> do
      m <- get
      outputStrLn $ concatMap displayPayment (reconcile . total $ sheet m)
      repl
    Just input -> do
      outputStrLn $ "*** " ++ input ++ " IS NOT A VALID COMMAND ***"
      repl

