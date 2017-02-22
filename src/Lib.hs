module Lib where

import           Data.Time.Calendar
import           Data.Map.Strict  (Map)
import qualified Data.Map.Strict as M
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as N

someFunc :: IO ()
someFunc = putStrLn "Hello Sheet"

type Name = String

data Event = Event
  { description  :: String
  , date         :: Day
  , payer        :: Name
  , participants :: NonEmpty Name 
  , amount       :: Rational
  } deriving (Show)

type Sheet = [Event]

type Entry = Map String Rational

mkLineItem :: Event -> Entry
mkLineItem e = M.unionWith (+) paid owesMap
  where
    paid = M.singleton (payer e) (amount e)
    owesMap = M.fromList owes
    owes = map (\p -> (p, (-1) * amount e / fromIntegral n)) (N.toList $ participants e)
    n    = length (participants e)

total :: Sheet -> Entry
total sheet = foldr (M.unionWith (+)) M.empty (mkLineItem <$> sheet)
