{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}


------------------------------------------------------------------------------
-- |
-- Module      :  Report
-- Copyright   :  (c) 2017 Jeffrey Rosenbluth
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  jeffrey.rosenbluth@gmail.com
--
-- Generate an html report of the sheet.
-------------------------------------------------------------------------------

module Report where

import           Sheet

import           Data.Monoid
import           Lucid
import qualified Data.Map.Strict    as M
import           Data.List          (sortOn)
import qualified Data.List.NonEmpty as N
import           Data.Text.Lazy     (Text)
import           Data.Time.Format   (defaultTimeLocale, formatTime)

 
header :: Html ()
header =  title_ "Sheet-it"
       <> meta_ [charset_ "utf-8"]
       <> link_ [rel_ "stylesheet"
                , type_ "text/css"
                , href_ "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
                ]

bodyHeader :: Html ()
bodyHeader =
  div_ $
    h1_ "Sheet-it"

report :: Sheet -> Html ()
report sheet =
  html_ $
       head_ header
    <> body_ (  bodyHeader
             <> div_ [class_ "col-xs-4"] (h2_ "Total" <> reportEntry entry)
             <> div_ [class_ "col-xs-6 col-xs-offset-2"] (h2_ "Payments" <> reportPayments pmts)
             <> div_ [class_ "col-xs-12"] (h2_ "Sheet" <> reportSheet sheet)
             )
  where
    entry = total sheet
    pmts  = reconcile entry

reportEntry :: Entry -> Html ()
reportEntry entry =
  table_ [class_ "table table-striped table-condensed"] $
         tr_ (th_ "Name" <> th_ [class_ "text-right"] "Amount")
      <> M.foldMapWithKey item entry
  where
    item :: String -> Rational -> Html ()
    item s r = tr_ (td_ (toHtml s) <> td_ [class_ "text-right"] (toHtml . displayRational $ r))

reportSheet :: Sheet -> Html ()
reportSheet s =
  table_ [class_ "table table-striped table-condensed"] $
         tr_ (th_ "Date" <> th_ "Description" <> th_ "Payer" <> th_ "Amount" <> th_ "Participants")
      <> (mconcat $ map reportEvent (sortOn date s))

reportEvent :: Event -> Html ()
reportEvent e =
  tr_ (  td_ dt
      <> td_ (toHtml $ description e)
      <> td_ (toHtml $ payer e)
      <> td_ amt
      <> td_ ps
      )
  where
    dt = toHtml $ formatTime defaultTimeLocale "%D " (date e)
    amt = toHtml $ displayRational (amount e)
    ps  = toHtml $ unwords (N.toList . N.sort $ participants e) 

reportPayment :: Payment -> Html ()
reportPayment p =
  tr_ ( td_ (toHtml $ from p)
     <> td_ (toHtml $ to p)
     <> td_ [class_ "text-right"] amt
      )
  where
    amt = toHtml $ displayRational (pmt p)

reportPayments :: [Payment] -> Html ()
reportPayments ps =
  table_ [class_ "table table-striped table-condensed"] $
         tr_ (th_ "From" <> th_ "To" <> th_ [class_ "text-right"] "Amount") 
      <> (mconcat $ map reportPayment ps)

renderReport :: Sheet -> Text
renderReport = renderText . report

