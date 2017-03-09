{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE StandaloneDeriving   #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-------------------------------------------------------------------------------
-- |
-- Module      :  Sheet
-- Copyright   :  (c) 2017 Jeffrey Rosenbluth
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  jeffrey.rosenbluth@gmail.com
--
-- Tools for storing and settling shared payments.
-------------------------------------------------------------------------------

module Sheet where

import           Control.Applicative          ((<|>))
import           Data.Char                    (isDigit)
import           Data.List                    (sortOn)
import           Data.Time.Calendar           (Day(..))
import           Data.Time.Format             (defaultTimeLocale, formatTime ,parseTimeM)
import           Data.Map.Strict              (Map)
import qualified Data.Map.Strict              as M
import           Data.List.NonEmpty           (NonEmpty)
import qualified Data.List.NonEmpty           as N
import           Data.Serialize               (Serialize)
import           GHC.Generics
import           Text.ParserCombinators.ReadP -- (ReadP, munch1, skipSpaces, char, pfail)
import           Text.Printf                  (printf)
import           Text.Read                    (readMaybe)

-- Types -----------------------------------------------------------------------

type Name = String

data Event = Event
  { ident        :: Int
  , description  :: String
  , date         :: Day
  , payer        :: Name
  , participants :: NonEmpty Name
  , amount       :: Rational
  } deriving (Show, Eq, Generic)

deriving instance Generic Day
instance Serialize Day
instance Serialize (NonEmpty Name)
instance Serialize Event

type Sheet = [Event]

type Entry = Map String Rational

data Payment = Payment
  { from :: Name
  , to   :: Name
  , pmt  :: Rational
  } deriving (Show, Eq)

data Command
  = New FilePath
  | Open FilePath
  | Show
  | Add Event
  | Delete Int
  | Total
  | Reconcile
  | Help
  | Quit
  deriving Show


-- To String -------------------------------------------------------------------

-- | Convert a rational nuber to a string with 2 decimal places.
displayRational :: Rational -> String
displayRational = printf "% 10.2f" . dbl
  where
    dbl :: Rational -> Double
    dbl = fromRational

-- | 'Payment' to String.
displayPayment :: Payment -> String
displayPayment (Payment f t p) =
  f ++ " -> " ++ t ++ ": " ++ displayRational p ++ "\n"

-- | 'Entry' to String.
displayEntry :: Entry -> String
displayEntry = M.foldlWithKey'
  (\b k v -> b ++ k ++ ": " ++ displayRational v ++ "\n") ""

-- | 'Event' to String
displayEvent :: Event -> String
displayEvent e
  =  printf "% 3d " (ident e)
  ++ formatTime defaultTimeLocale "%D " (date e)
  ++ printf "%-31.30s" (description e)
  ++ printf "%-5.4s" (payer e)
  ++ printf "% 10.2f " (fromRational (amount e) :: Double)
  ++ unwords (N.toList . N.sort $ participants e) ++ "\n"

-- | 'Sheet' to String, sorted by date.
displaySheet :: Sheet -> String
displaySheet = concatMap displayEvent . sortOn date

-- Sheet Logic -----------------------------------------------------------------

-- | Convert a line item into an entry in the 'Sheet' (ledger).
mkLineItem :: Event -> Entry
mkLineItem e = M.unionWith (+) paid (M.fromList owes)
  where
    paid = M.singleton (payer e) (amount e)
    owes = map (\p -> (p, (-1) * amount e / fromIntegral n))
               (N.toList $ participants e)
    n    = length (participants e)

-- | Tally up all of the debts in the sheet.
total :: Sheet -> Entry
total sheet = foldr (M.unionWith (+)) M.empty (mkLineItem <$> sheet)

-- | Find the smallest value in a map if it exists.
minValue :: (Ord v, Ord k) => Map k v -> Maybe (k, v)
minValue m
  | null m    = Nothing
  | otherwise = Just $ M.foldrWithKey' f (k1, v1) m
  where
    f k v (k0, v0) = if v < v0  then (k, v) else (k0, v0)
    (k1, v1) = M.elemAt 0 m

-- | Find the largets value in a map if it exists.
maxValue :: (Ord v, Ord k) => Map k v -> Maybe (k, v)
maxValue m
  | null m    = Nothing
  | otherwise = Just $ M.foldrWithKey' f (k1, v1) m
  where
    f k v (k0, v0) = if v > v0  then (k, v) else (k0, v0)
    (k1, v1) = M.elemAt 0 m

-- | Pair the largest debtor with the largest lender and ajust the 'Sheet' (ledger).
pairOff :: Entry -> Maybe (Payment, Entry)
pairOff e = do
  (f, a) <- maxValue e
  (t, b) <- minValue e
  return $
    if a >= abs b
      then (Payment t f (negate b), M.delete t . M.adjust (+ b) f $ e)
      else (Payment t f a         , M.adjust (+ a) t . M.delete f $ e)

-- | Create a list of transfer payments so that there are no debts left in the 'Sheet'.
reconcile :: Entry -> [Payment]
reconcile e = if M.size e >= 2
  then
    case pairOff e of
      Nothing -> error "THE SKY IS FALLING, TRIED TO PAIR OFF AN EMPTY MAP."
      Just (p, e') -> p : reconcile e'
  else []

-- | Remove the 'Event' with the given indentifier from the 'Sheet'
deleteEntry :: Sheet -> Int -> Sheet
deleteEntry s n = filter ((/= n) . ident) s

-- Parser ----------------------------------------------------------------------

parseCommand :: ReadP Command
parseCommand
  =   parseNew
  <|> parseOpen
  <|> parseShow
  <|> parseAdd
  <|> parseDel
  <|> parseTot
  <|> parseRec
  <|> parseHelp
  <|> parseQuit

symbol :: String -> ReadP String
symbol s = string s <* skipSpaces

parseNew :: ReadP Command
parseNew = New <$> (symbol "new" *> notComma)

parseOpen :: ReadP Command
parseOpen = Open <$> (symbol "open" *> notComma)

parseShow :: ReadP Command
parseShow = Show <$ symbol "show"

parseAdd :: ReadP Command
parseAdd = Add <$> (symbol "+" *> parseEvent)

parseDel :: ReadP Command
parseDel = Delete <$> (symbol "delete" *> posInt)

parseTot :: ReadP Command
parseTot = Total <$ symbol "total"

parseRec :: ReadP Command
parseRec = Reconcile <$ symbol "reconcile"

parseHelp :: ReadP Command
parseHelp = Help <$ symbol "help"

parseQuit :: ReadP Command
parseQuit = Quit <$ symbol "quit"

-- | Convert an event string to an 'Event'.
parseEvent :: ReadP Event
parseEvent = do
  dt   <- parseDay <* optional comma <* skipSpaces
  desc <- notComma <* sep
  pyr  <- notComma <* sep
  amt  <- parseAmount <* sep
  ps   <- parseParticipants
  return $ Event 0 desc dt pyr ps amt

-- | parse a Date.
parseDay :: ReadP Day
parseDay = parseTimeM True defaultTimeLocale "%-m/%-d/%-y"
       =<< munch1 (\c -> c /= ',' && c /= ' ')

-- | Parse a rational number.
parseAmount :: ReadP Rational
parseAmount = do
  s <- notComma
  let x = readMaybe s :: Maybe Double
  case x of
    Nothing -> pfail
    Just y  -> return $ toRational (100 * y) / 100

-- | Parse a non-empty list of 'Name's.
parseParticipants :: ReadP (NonEmpty Name)
parseParticipants = do
  ns <- notComma
  return $ N.fromList (words ns)

-- | Parse a comma.
comma :: ReadP Char
comma = char ','

-- | Parse a forward slash.
slash :: ReadP Char
slash = char '/'

-- | Parse until reaching a comma.
notComma :: ReadP String
notComma = munch1 (/= ',')

-- | Separator parser.
sep :: ReadP Char
sep = comma <* skipSpaces

-- | Parse a positive Int.
posInt :: ReadP Int
posInt = read <$> many1 (satisfy isDigit)
