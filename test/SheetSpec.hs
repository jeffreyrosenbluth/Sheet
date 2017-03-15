module SheetSpec (spec) where

import           Sheet
import           Test.Hspec
import           Data.Time.Calendar
import qualified Data.Map.Strict as M
import qualified Data.List.NonEmpty as N
import           Data.Ratio ((%))
import           Data.Serialize
import           Text.ParserCombinators.ReadP

toInitials :: String -> Initials
toInitials (a:b:[]) = (a, b)
toInitials _ = error "Must be a two character list"
 
eventString1 :: String
eventString1 = "2/27/17, Bowling, JR, 100.10, JR VH AK SZ SO"
 
event1 :: Event
event1 = Event 0
               "Bowling"
               (fromGregorian 2017 2 27)
               (toInitials "JR")
               (N.fromList $ map toInitials ["JR", "VH", "AK", "SZ", "SO"])
               100.10

event2 :: Event
event2 = Event 1
               "Bowling"
               (fromGregorian 2017 2 27)
               (toInitials "JR")
               (N.fromList $ map toInitials ["VH", "AK", "SZ", "SO"])
               100.10

entry1 :: Entry
entry1 =
  M.fromList [ (toInitials "JR", 80.08)
             , (toInitials "VH", -20.02)
             , (toInitials "AK", -20.02)
             , (toInitials "SZ", -20.02)
             , (toInitials "SO", -20.02)
             ]

entry1' :: Entry
entry1' =
  M.fromList [ (toInitials "JR", -80.08)
             , (toInitials "VH", 20.02)
             , (toInitials "AK", 20.02)
             , (toInitials "SZ", 20.02)
             , (toInitials "SO", 20.02)
             ]

entry2 :: Entry
entry2 =
  M.fromList [ (toInitials "JR", 100.10)
             , (toInitials "VH", -25.025)
             , (toInitials "AK", -25.025)
             , (toInitials "SZ", -25.025)
             , (toInitials "SO", -25.025)
             ]

entry12 :: Entry
entry12 =
  M.fromList [ (toInitials "JR", 180.18)
             , (toInitials "VH", -45.045)
             , (toInitials "AK", -45.045)
             , (toInitials "SZ", -45.045)
             , (toInitials "SO", -45.045)
             ]

entry3a :: Entry
entry3a = M.fromList [(toInitials "AX", -17), (toInitials "BY", 34), (toInitials "CZ", -17)]

entry3b :: Entry
entry3b = M.fromList [(toInitials "AX", 10), (toInitials "BY", -5), (toInitials "DU", -5)]

entry3ab :: Entry
entry3ab = M.fromList [(toInitials "AX", -7), (toInitials "BY", 29), (toInitials "CZ", -17), (toInitials "DU", -5)]

pairOff1 :: (Payment, Entry)
pairOff1 = ( Payment {from = toInitials "AK", to = toInitials "JR", pmt = 1001 % 50}
           , M.fromList [ (toInitials "JR",3003 % 50)
                        , (toInitials "SO",(-1001) % 50)
                        , (toInitials "SZ",(-1001) % 50)
                        , (toInitials "VH",(-1001) % 50)
                        ]
           )

pairOff1' :: (Payment, Entry)
pairOff1' = ( Payment {from = toInitials "JR", to = toInitials "AK", pmt = 1001 % 50}
           , M.fromList [ (toInitials "JR",(-3003) % 50)
                        , (toInitials "SO",1001 % 50)
                        , (toInitials "SZ",1001 % 50)
                        , (toInitials "VH",1001 % 50)
                        ]
           )

spec :: Spec
spec = do
  describe "mkLineItem" $ 
    it "Makes a dictionary from an event including payer" $ do
      mkLineItem event1 `shouldBe` entry1
      mkLineItem event2 `shouldBe` entry2
  describe "total" $ 
    it "Totals a list of events" $
      total [event1, event2] `shouldBe` entry12
  describe "serialize" $ 
    it "Checks that (decode . encodei) == id" $
      (decode . encode $ [entry1, entry2]) `shouldBe` Right [entry1, entry2]
  describe "Parse" $  do
    it "Parses a date" $ do
      readP_to_S parseDay "02/01/17" `shouldBe` [(fromGregorian 2017 2 1, "")]
      readP_to_S parseDay "2/1/17" `shouldBe` [(fromGregorian 2017 2 1, "")]
    it "Parses a string to an event" $
      readP_to_S parseEvent eventString1 `shouldBe` [(event1, "")]
  describe "pariOff" $ do
    it "Pairs off the biggest debtor with the biggest lender" $ do
      pairOff entry1 `shouldBe` Just pairOff1
      pairOff entry1' `shouldBe` Just pairOff1'
  describe "reconcile" $ do
    it "There are 4 payments" $ do
      length (reconcile entry1) `shouldBe` 4
      length (reconcile entry1') `shouldBe` 4
    it "Payment is 20.02" $ do
      (pmt . head $ reconcile entry1) `shouldBe` 20.02
      (pmt . head $ reconcile entry1') `shouldBe` 20.02
    it "Debtor and lender are correct" $ do
      (to . head $ reconcile entry1) `shouldBe` toInitials "JR"
      (from . head $ reconcile entry1') `shouldBe` toInitials "JR"
