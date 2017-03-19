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

import           Data.List                    (sortOn)
import           Data.Time.Calendar           (Day(..))
import           Data.Time.Format             (defaultTimeLocale, formatTime)
import           Data.Map.Strict              (Map)
import qualified Data.Map.Strict              as M
import           Data.List.NonEmpty           (NonEmpty)
import qualified Data.List.NonEmpty           as N
import           Data.Monoid                  ((<>))
import           Data.Serialize               (Serialize)
import           GHC.Generics
import           Text.Printf                  (printf)

-- Types -----------------------------------------------------------------------

type Initials = (Char, Char)

data Event = Event
  { ident        :: Int
  , description  :: String
  , date         :: Day
  , payer        :: Initials
  , participants :: NonEmpty Initials
  , amount       :: Rational
  } deriving (Show, Eq, Generic)

deriving instance Generic Day
instance Serialize Day
instance Serialize (NonEmpty Initials)
instance Serialize Event

type Sheet = [Event]

type Entry = Map Initials Rational

data Payment = Payment
  { from :: Initials
  , to   :: Initials
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
  | Report  FilePath
  | Quit
  deriving Show

-- To String -------------------------------------------------------------------

-- | Convert 'Initials' to a string.
displayInitials :: Initials -> String
displayInitials (f, l) = [f, l]

-- | Convert a rational number to a string with 2 decimal places.
displayRational :: Rational -> String
displayRational = printf "% 10.2f" . dbl
  where
    dbl :: Rational -> Double
    dbl = fromRational

-- | 'Payment' to String.
displayPayment :: Payment -> String
displayPayment (Payment f t p) =
  displayInitials f <> " -> " <> displayInitials t <> ": " <> displayRational p <> "\n"

-- | 'Entry' to String.
displayEntry :: Entry -> String
displayEntry = M.foldlWithKey'
  (\b k v -> b <> displayInitials k <> ": " <> displayRational v <> "\n") ""

-- | 'Event' to String
displayEvent :: Event -> String
displayEvent e
  =  printf "% 3d. " (ident e)
  <> formatTime defaultTimeLocale "%D " (date e)
  <> printf "%-31.30s" (description e)
  <> printf "%-5.4s" (displayInitials $ payer e)
  <> printf "% 10.2f " (fromRational (amount e) :: Double)
  <> unwords (N.toList . N.sort $ (displayInitials <$> participants e)) <> "\n"

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

-- | Choose an extreme value from a map.
extremum :: Ord k => (v -> v -> Bool) -> Map k v -> Maybe (k, v)
extremum comp m
  | null m    = Nothing
  | otherwise = Just $ M.foldrWithKey' f (k1, v1) m
  where
    f k v (k0, v0) = if comp v v0  then (k, v) else (k0, v0)
    (k1, v1) = M.elemAt 0 m

-- | Find the smallest value in a map if it exists.
minValue :: (Ord v, Ord k) => Map k v -> Maybe (k, v)
minValue = extremum (<)

-- | Find the largets value in a map if it exists.
maxValue :: (Ord v, Ord k) => Map k v -> Maybe (k, v)
maxValue = extremum (>)

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

validateEvent :: Event -> Either String Event
validateEvent e@(Event _ _ _ _ ps _)
  | N.length ps /= (N.length . N.nub) ps = Left "dUPLICATE IN PARTICIPANT LIST"
  | otherwise = Right e
