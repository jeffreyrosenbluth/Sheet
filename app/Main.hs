{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -fno-warn-orphans  #-}

-------------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) 2017 Jeffrey Rosenbluth
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  jeffrey.rosenbluth@gmail.com
--
-- Command Line interface for storing and settling shared payments.
-------------------------------------------------------------------------------

module Main where

import           Sheet

import           Control.Monad.State.Strict
import qualified Data.ByteString as BS
import           Data.Serialize (encode, decode)
import           System.Console.Haskeline
import           System.Directory (doesFileExist)
import           Text.ParserCombinators.ReadP (readP_to_S)
import           Text.Read (readMaybe)

-- Types -----------------------------------------------------------------------

data Model = Model
  { nextId :: Int
  , sheet  :: Sheet
  , file   :: String
  }

data Command
  = New String
  | Open String
  | Show
  | Total
  | Add String
  | Delete String
  | Reconcile
  | Err String

--------------------------------------------------------------------------------

main :: IO ((), Model)
main = runStateT (runInputT defaultSettings repl) (Model 0 [] "")

parseCommand :: String -> Command
parseCommand s = case words s of
  ["new", fname]  -> New fname
  ["open", fname] -> Open fname
  ["show"]        -> Show
  ["total"]       -> Total
  "+" : str       -> Add $ unwords str
  ["delete", str] -> Delete str
  ["reconcile"]   -> Reconcile
  err             -> Err $ unwords err

setModel :: [Event] -> String -> Model
setModel events = Model (maximum $ map ident events) events

addEvent :: Event -> Model -> Model
addEvent e m = m {sheet = (e {ident = n}) : sheet m, nextId = n}
  where
    n = nextId m + 1

instance MonadState s m => MonadState s (InputT m) where
  get   = lift get
  put   = lift . put
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
    Nothing     -> outputStrLn "Goodbye."
    Just "quit" -> outputStrLn "Goodbye"
    Just input  -> process (parseCommand input) >> repl

process :: Command -> Repl ()
process Show      = get >>= outputStrLn . displaySheet . sheet
process Total     = get >>= outputStrLn . displayEntry . total . sheet
process Reconcile = get >>= outputStrLn . concatMap displayPayment . reconcile . total . sheet
process (New fname) = modify (\m -> m {file = fname})
process (Open fname) = do
    exists <- liftIO $ doesFileExist fname
    if exists
      then do
        efile <- liftIO $ BS.readFile fname
        case decode efile of
          Left err     -> outputStrLn $ "*** CANNOT DECODE FILE: " ++ err ++ " ***"
          Right events -> put $ setModel events fname
      else
        outputStrLn $ "*** FILE: " ++ fname ++ " DOES NOT EXIST. ***"
process (Add eventString) =
    case readP_to_S parseEvent eventString of
      (e, _) : _ -> modify (addEvent e) >> save
      []         -> outputStrLn "*** INVALID EVENT ***"
process (Delete n) =
    case readMaybe n of
      Nothing -> outputStrLn $ "*** PARSE ERROR " ++ n ++ " IS NOT AN INTEGER ***"
      Just n' -> modify (\m -> m {sheet = deleteEntry (sheet m) n'}) >> save
process (Err err) = outputStrLn $ "*** " ++ err ++ " IS NOT A VALID COMMAND ***"
