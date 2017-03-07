{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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

setModel :: [Event] -> String -> Model
setModel events = Model (maximum $ map ident events) events

addEvent :: Event -> Model -> Model
addEvent e m = m {sheet = (e {ident = n}) : sheet m, nextId = n}
  where
    n = nextId m + 1

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
    Nothing     -> outputStrLn "Goodbye."
    Just "quit" -> outputStrLn "Goodbye"
    Just input  -> process input >> repl

process :: String -> Repl ()
process str = case words str of
  ["new", fname] -> modify (\m -> m {file = fname})

  ["open", fname] -> do
    exists <- liftIO $ doesFileExist fname
    if exists
      then do
        efile <- liftIO $ BS.readFile fname
        case decode efile of
          Left err     -> outputStrLn $ "*** CANNOT DECODE FILE: " ++ err ++ " ***"
          Right events -> put $ setModel events fname
      else
        outputStrLn $ "*** FILE: " ++ fname ++ " DOES NOT EXIST. ***"

  ["show"]  -> get >>= outputStrLn . displaySheet . sheet

  ["total"] -> get >>= outputStrLn . displayEntry . total . sheet

  "+" : eventString ->
    case readP_to_S parseEvent (concat eventString) of
      (e, _) : _ -> modify (addEvent e) >> save
      []         -> outputStrLn "*** INVALID EVENT ***"

  ["delete", n] ->
    case readMaybe n of
      Nothing -> outputStrLn $ "*** EVENT " ++ n ++ " DOES NOT EXIST ***"
      Just n' -> modify (\m -> m {sheet = deleteEntry (sheet m) n'}) >> save

  ["reconcile"] -> get >>= outputStrLn . concatMap displayPayment . reconcile . total . sheet

  err -> outputStrLn $ "*** " ++ unwords err ++ " IS NOT A VALID COMMAND ***"

