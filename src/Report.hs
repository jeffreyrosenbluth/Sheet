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
header =  title_ "Sheet it"
       <> meta_ [charset_ "utf-8"]

bodyHeader :: Html ()
bodyHeader =
  div_ $
    h1_ "The Sheet"

report :: Sheet -> Html ()
report sheet =
  html_ $
       head_ header
    <> body_ (  bodyHeader
             <> reportEntry entry
             <> reportSheet sheet
             )
  where
    entry = total sheet

reportEntry :: Entry -> Html ()
reportEntry entry = ul_ $ M.foldMapWithKey item entry
  where
    item :: String -> Rational -> Html ()
    item s r = li_ $ toHtml (s <> (displayRational r))

reportSheet :: Sheet -> Html ()
reportSheet s =
  div_ $ table_ (mconcat $ map reportEvent (sortOn date s))

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
   


renderReport :: Sheet -> Text
renderReport = renderText . report

