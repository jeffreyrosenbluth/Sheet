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
import           Report

import           Control.Monad.State.Strict
import qualified Data.ByteString as BS
import           Data.Serialize (encode, decode)
import qualified Data.Text.Lazy.IO           as T 
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

--------------------------------------------------------------------------------

main :: IO ((), Model)
main = do
  putStrLn logo
  putStrLn "Copyright 2017, Jeffrey Rosenbluth"
  putStrLn "Version 0.1\n"
  runStateT (runInputT defaultSettings repl) (Model 0 [] "")

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
    Nothing   -> outputStrLn "Goodbye"
    Just cmd  ->
      case readP_to_S parseCommand cmd of
        []            -> outputStrLn ("*** UNABLE TO PARSE COMMAND " ++ cmd ++ " ***") >> repl
        (Quit, _) : _ -> outputStrLn "Goodbye"
        (c, _)    : _ -> process c


process :: Command -> Repl ()
process Show          = get >>= outputStrLn . displaySheet . sheet >> repl
process Total         = get >>= outputStrLn . displayEntry . total . sheet >> repl
process Reconcile     = get >>= outputStrLn . concatMap displayPayment
                                           . reconcile . total . sheet >> repl
process Help          = outputStrLn help >> repl
process (Print fname) = get >>= liftIO . T.writeFile fname . renderReport . sheet >> repl
process (New fname)   = modify (\m -> m {file = fname}) >> repl
process (Add event )  = modify (addEvent event) >> save >> repl
process (Delete n)    = modify (\m -> m {sheet = deleteEntry (sheet m) n}) >> save >> repl
process Quit          = outputStrLn "Goodbye"
process (Open fname)  = do
    exists <- liftIO $ doesFileExist fname
    if exists
      then do
        efile <- liftIO $ BS.readFile fname
        case decode efile of
          Left err     -> outputStrLn $ "*** CANNOT DECODE FILE: " ++ err ++ " ***"
          Right events -> put $ setModel events fname
      else
        outputStrLn $ "*** FILE: " ++ fname ++ " DOES NOT EXIST. ***"
    repl

-- Logo ------------------------------------------------------------------------

logo :: String
logo =  "  _____ _               _       _   \n"
     ++ " / ____| |             | |   (_) |  \n"
     ++ "| (___ | |__   ___  ___| |_   _| |_ \n"
     ++ " \\___ \\| '_ \\ / _ \\/ _ \\ __| | | __|\n"
     ++ " ____) | | | |  __/  __/ |_  | | |_ \n"
     ++ "|_____/|_| |_|\\___|\\___|\\__| |_|\\__|\n"

-- Help ------------------------------------------------------------------------

help :: String
help
  =  "\nCommand                       Description\n"
  ++ "---------------               ------------------------------------------\n"
  ++ "help                          This page\n"
  ++ "new <filename>                Start a new sheet\n"
  ++ "open <filename>               Open an existing sheet\n"
  ++ "show                          Display the sheet on the screen\n"
  ++ "print <filename>              Output the results to a file in html\n"
  ++ "delete <n>                    Delete entry with key n\n"
  ++ "+ <entry>                     Add an entry where <entry> is\n"
  ++ "                                <dd/mm/yy> <description>, <payer>,\n"
  ++ "                                <amount>, <participants>\n"
  ++ "                                example: + 7/04/16, Dinner at McDonalds,\n"
  ++ "                                           JR, 75.62, JR RS JT DM\n"
  ++ "total                         Aggregate all payments\n"
  ++ "reconcile                     Pairoff borrowers and lenders\n"
  ++ "quit                          \n"
