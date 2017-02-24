module LibSpec (spec) where

import           Lib
import           Test.Hspec
import           Data.Time.Calendar
import qualified Data.Map.Strict as M
import qualified Data.List.NonEmpty as N
import           Data.Serialize
import           Text.ParserCombinators.ReadP

eventString1 = "2/27/17, Bowling, JR, 100.10, JR VH AK SZ SO"
 
event1 :: Event
event1 = Event "Bowling"
               (fromGregorian 2017 2 27)
               "JR"
               (N.fromList ["JR", "VH", "AK", "SZ", "SO"])
               100.10

event2 :: Event
event2 = Event "Bowling"
               (fromGregorian 2017 2 27)
               "JR"
               (N.fromList ["VH", "AK", "SZ", "SO"])
               100.10

dict1 :: Entry
dict1 =
  M.fromList [ ("JR", 80.08)
             , ("VH", -20.02)
             , ("AK", -20.02)
             , ("SZ", -20.02)
             , ("SO", -20.02)
             ]

dict1' :: Entry
dict1' =
  M.fromList [ ("JR", -80.08)
             , ("VH", 20.02)
             , ("AK", 20.02)
             , ("SZ", 20.02)
             , ("SO", 20.02)
             ]

dict2 :: Entry
dict2 =
  M.fromList [ ("JR", 100.10)
             , ("VH", -25.025)
             , ("AK", -25.025)
             , ("SZ", -25.025)
             , ("SO", -25.025)
             ]

dict12 :: Entry
dict12 =
  M.fromList [ ("JR", 180.18)
             , ("VH", -45.045)
             , ("AK", -45.045)
             , ("SZ", -45.045)
             , ("SO", -45.045)
             ]

spec :: Spec
spec = do
  describe "mkLineItem" $ do
    it "Makes a dictionary from an event including payer" $
      mkLineItem event1 `shouldBe` dict1
    it "Makes a dictionary from an event excluding payer" $
      mkLineItem event2 `shouldBe` dict2
  describe "total" $ do
    it "Totals a list of events" $
      total [event1, event2] `shouldBe` dict12
  describe "serialize" $ do
    it "Checks that (decode . encodei) == id" $
      (decode . encode $ [dict1, dict2]) `shouldBe` Right [dict1, dict2]
  describe "parseEvent" $ do
    it "Parsers a string to an event" $
      readP_to_S parseEvent eventString1 `shouldBe` [(event1, "")]
  describe "pariOff" $ do
    it "Pairs off the biggest debtor with the biggest lender" $
      print $ pairOff dict1
    it "Pairs off the biggest debtor with the biggest lender" $
      print $ pairOff dict1'
