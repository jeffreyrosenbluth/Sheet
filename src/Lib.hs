{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Lib where

import           Data.Char
import           Data.Time.Calendar
import           Data.Time.Format
import           Data.Map.Strict  (Map)
import qualified Data.Map.Strict as M
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as N
import           Data.Serialize 
import           GHC.Generics
import           Text.ParserCombinators.ReadP
import           Text.Printf

type Name = String

data Event = Event
  { description  :: String
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

displayEntry :: Entry -> String
displayEntry = M.foldlWithKey'
  (\b k v -> b ++ k ++ ": " ++ (printf "% 10.2f\n" . dbl) v) ""
  where
    dbl :: Rational -> Double
    dbl = fromRational

displayEvent :: Event -> String
displayEvent e
  =  formatTime defaultTimeLocale "%D " (date e)
  ++ printf "%-31.30s" (description e)
  ++ printf "%-5.4s" (payer e)
  ++ printf "% 10.2f " (fromRational (amount e) :: Double)
  ++ unwords (N.toList $ participants e) ++ "\n"

displaySheet :: Sheet -> String
displaySheet = concatMap displayEvent

mkLineItem :: Event -> Entry
mkLineItem e = M.unionWith (+) paid owesMap
  where
    paid = M.singleton (payer e) (amount e)
    owesMap = M.fromList owes
    owes = map (\p -> (p, (-1) * amount e / fromIntegral n))
               (N.toList $ participants e)
    n    = length (participants e)

total :: Sheet -> Entry
total sheet = foldr (M.unionWith (+)) M.empty (mkLineItem <$> sheet)

-- Parser ----------------------------------------------------------------------

parseEvent :: ReadP Event
parseEvent = do
  dt   <- parseDate <* sep
  desc <- notComma <* sep
  pyr  <- notComma <* sep
  amt  <- parseAmount <* sep
  ps   <- parseParticipants
  return $ Event desc dt pyr ps amt

parseDate :: ReadP Day
parseDate = do
  month <- posInt <* slash
  day   <- posInt <* slash
  year  <- parseYear
  return $ fromGregorian year month day

parseAmount :: ReadP Rational
parseAmount = do
  s <- notComma
  return $ (toRational (100 * read s :: Double)) / 100

parseParticipants :: ReadP (NonEmpty Name)
parseParticipants = do
  ns <- notComma
  return $ N.fromList (words ns)

comma :: ReadP Char
comma = char ','

slash :: ReadP Char
slash = char '/'

notComma :: ReadP String
notComma = munch1 (/= ',')

sep :: ReadP Char
sep = comma <* skipSpaces

posInt :: ReadP Int
posInt = readS_to_P f
  where
    f xs
      | null ns   = []
      | otherwise = [(read ns, rest)]
      where (ns, rest) = span isDigit xs

parseYear :: ReadP Integer
parseYear = readS_to_P f
  where
    f xs
      | null ns   = []
      | otherwise = [(2000 + read ns, rest)]
      where (ns, rest) = span isDigit xs
