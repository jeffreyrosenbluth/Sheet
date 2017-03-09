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
import qualified Data.Map.Strict as M
import           Data.Text.Lazy (Text)

 
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
    <> body_ (bodyHeader <> reportEntry entry)
  where
    entry = total sheet

reportEntry :: Entry -> Html ()
reportEntry entry = ul_ $ M.foldMapWithKey item entry
  where
    item :: String -> Rational -> Html ()
    item s r = li_ $ toHtml (s <> (displayRational r))

renderReport :: Sheet -> Text
renderReport = renderText . report
 
