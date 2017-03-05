module LibSpec (spec) where

import           Lib
import           Test.Hspec
import           Data.Time.Calendar
import qualified Data.Map.Strict as M
import qualified Data.List.NonEmpty as N
import           Data.Ratio ((%))
import           Data.Serialize
import           Text.ParserCombinators.ReadP

eventString1 = "2/27/17, Bowling, JR, 100.10, JR VH AK SZ SO"
 
event1 :: Event
event1 = Event 0
               "Bowling"
               (fromGregorian 2017 2 27)
               "JR"
               (N.fromList ["JR", "VH", "AK", "SZ", "SO"])
               100.10

event2 :: Event
event2 = Event 1
               "Bowling"
               (fromGregorian 2017 2 27)
               "JR"
               (N.fromList ["VH", "AK", "SZ", "SO"])
               100.10

entry1 :: Entry
entry1 =
  M.fromList [ ("JR", 80.08)
             , ("VH", -20.02)
             , ("AK", -20.02)
             , ("SZ", -20.02)
             , ("SO", -20.02)
             ]

entry1' :: Entry
entry1' =
  M.fromList [ ("JR", -80.08)
             , ("VH", 20.02)
             , ("AK", 20.02)
             , ("SZ", 20.02)
             , ("SO", 20.02)
             ]

entry2 :: Entry
entry2 =
  M.fromList [ ("JR", 100.10)
             , ("VH", -25.025)
             , ("AK", -25.025)
             , ("SZ", -25.025)
             , ("SO", -25.025)
             ]

entry12 :: Entry
entry12 =
  M.fromList [ ("JR", 180.18)
             , ("VH", -45.045)
             , ("AK", -45.045)
             , ("SZ", -45.045)
             , ("SO", -45.045)
             ]

entry3a :: Entry
entry3a = M.fromList [("AX", -17), ("BY", 34), ("CZ", -17)]

entry3b :: Entry
entry3b = M.fromList [("AX", 10), ("BY", -5), ("DU", -5)]

entry3ab :: Entry
entry3ab = M.fromList [("AX", -7), ("BY", 29), ("CZ", -17), ("DU", -5)]

pairOff1 :: (Payment, Entry)
pairOff1 = ( Payment {from = "AK", to = "JR", pmt = 1001 % 50}
           , M.fromList [ ("AK",0 % 1)
                      , ("JR",3003 % 50)
                      , ("SO",(-1001) % 50)
                      , ("SZ",(-1001) % 50)
                      , ("VH",(-1001) % 50)
                      ]
           )

pairOff1' :: (Payment, Entry)
pairOff1' = ( Payment {from = "JR", to = "AK", pmt = 1001 % 50}
           , M.fromList [ ("AK",0 % 1)
                      , ("JR",(-3003) % 50)
                      , ("SO",1001 % 50)
                      , ("SZ",1001 % 50)
                      , ("VH",1001 % 50)
                      ]
           )

spec :: Spec
spec = do
  describe "mkLineItem" $ do
    it "Makes a dictionary from an event including payer" $
      mkLineItem event1 `shouldBe` entry1
    it "Makes a dictionary from an event excluding payer" $
      mkLineItem event2 `shouldBe` entry2
  describe "total" $ do
    it "Totals a list of events" $
      total [event1, event2] `shouldBe` entry12
  describe "serialize" $ do
    it "Checks that (decode . encodei) == id" $
      (decode . encode $ [entry1, entry2]) `shouldBe` Right [entry1, entry2]
  describe "parseEvent" $ do
    it "Parsers a string to an event" $
      readP_to_S parseEvent eventString1 `shouldBe` [(event1, "")]
  describe "pariOff" $ do
    it "Pairs off the biggest debtor with the biggest lender" $
      pairOff entry1 `shouldBe` pairOff1
    it "Pairs off the biggest lender with the biggest debtor" $
      pairOff entry1' `shouldBe` pairOff1'
  describe "reconcile" $ do
    it "There are 4 payments" $ do
      length (reconcile entry1) `shouldBe` 4
      length (reconcile entry1') `shouldBe` 4
    it "Payment is 20.02" $ do
      (pmt . head $ reconcile entry1) `shouldBe` 20.02
      (pmt . head $ reconcile entry1') `shouldBe` 20.02
    it "JR is the lender" $
      (to . head $ reconcile entry1) `shouldBe` "JR"
    it "JR is the lender" $
      (from . head $ reconcile entry1') `shouldBe` "JR"
