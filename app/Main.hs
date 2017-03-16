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

import           Control.Monad                (unless)
import           Control.Monad.State.Strict
import qualified Data.ByteString              as BS
import           Data.Serialize               (encode, decode)
import qualified Data.Text.Lazy.IO            as T
import           System.Console.Haskeline
import           System.Directory             (doesFileExist)
import           System.Environment           (getArgs)
import           Text.ParserCombinators.ReadP (readP_to_S)

-- Types -----------------------------------------------------------------------

data Model = Model
  { prevId :: Int
  , sheet  :: Sheet
  , file   :: FilePath
  }

--------------------------------------------------------------------------------

main :: IO ((), Model)
main = do
  putStrLn logo
  putStrLn "Copyright 2017, Jeffrey Rosenbluth"
  putStrLn "Version 0.1\n"
  args <- getArgs
  case args of
    [] -> runStateT (runInputT sheetSettings repl) (Model 0 [] "")
    (fname:_) -> do
      (m, err) <- openSheet fname
      putStrLn err
      runStateT (runInputT sheetSettings repl) m
   where
     sheetSettings = defaultSettings {historyFile = Just "sheetit_history.txt"}

-- | Add an extension to a 'FilePath' if it does not alread have one.
withExt :: FilePath -> FilePath -> FilePath
withExt ext base
  | '.' `elem` base = base
  | otherwise       = base ++ "." ++ ext

-- | Given a sheet, return a function which takes a 'FilePath' and returns a Model.
--   The 'prevId' field is set to the maximum of the ones already in the map.
setModel :: Sheet -> String -> Model
setModel events = Model (maximum $ map ident events) events

-- | Decode a sheet and use it and the file name to set the model.
--   Return the 'Model' along with a message indicating success or failure.
openSheet :: FilePath -> IO (Model, String)
openSheet fname = do
  let fn = withExt "sht" fname
  exists <- doesFileExist fn
  if exists
    then do
      efile <- BS.readFile fn
      return $ case decode efile of
        Left msg     -> (Model 0 [] "", "*** CANNOT DECODE FILE: " ++ msg ++ " ***")
        Right events -> (setModel events fn, "*** " ++ fn ++ " LOADED SUCCESSFULLY ***")
    else
      return (Model 0 [] "", "*** FILE: " ++ fn ++ " DOES NOT EXIST. ***")

addEvent :: Event -> Model -> Model
addEvent e m = m {sheet = (e {ident = n}) : sheet m, prevId = n}
  where
    n = prevId m + 1

-- | We make InputT m an instance of 'MonadState' so we don't have to constantly lift get, put etc.
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
process  Show          = get >>= outputStrLn . displaySheet . sheet >> repl
process  Total         = get >>= outputStrLn . displayEntry . total . sheet >> repl
process  Reconcile     = get >>= outputStrLn . concatMap displayPayment
                                             . reconcile . total . sheet >> repl
process  Help          = outputStrLn help >> repl
process (Report fname) = get >>= liftIO . T.writeFile (withExt "html" fname)
                                        . renderReport . sheet >> repl
process (New fname)    = modify (\m -> m {file = (withExt "sht" fname)}) >> repl
process (Add event)    = modify (addEvent event) >> save >> repl
process (Delete n)     = modify (\m -> m {sheet = deleteEntry (sheet m) n}) >> save >> repl
process  Quit          = outputStrLn "Goodbye"
process (Open fname)   = do
  (m, msg) <- liftIO $ openSheet fname
  outputStrLn msg
  unless (null . sheet $ m) (put m)
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
  ++ "report <filename>             Output the results to a file in html\n"
  ++ "delete <n>                    Delete entry with key n\n"
  ++ "+ <entry>                     Add an entry where <entry> is\n"
  ++ "                                <dd/mm/yy> <description>, <payer>,\n"
  ++ "                                <amount>, <participants>\n"
  ++ "                                example: + 7/04/16, Dinner at McDonalds,\n"
  ++ "                                           JR, 75.62, JR RS JT DM\n"
  ++ "total                         Aggregate all payments\n"
  ++ "reconcile                     Pairoff borrowers and lenders\n"
  ++ "quit                          \n"
