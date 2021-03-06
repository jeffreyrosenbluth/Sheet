-------------------------------------------------------------------------------
-- |
-- Module      :  Parser
-- Copyright   :  (c) 2017 Jeffrey Rosenbluth
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  jeffrey.rosenbluth@gmail.com
--
-- Parse the user commands.
-------------------------------------------------------------------------------

module Parser where

import           Sheet
import           Control.Applicative          ((<|>))
import           Data.Time.Calendar           (Day(..))
import           Data.List.NonEmpty           (NonEmpty)
import qualified Data.List.NonEmpty as N
import           Text.ParserCombinators.ReadP
import           Data.Time.Format             (defaultTimeLocale, parseTimeM)

-- | parse a command.
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
  <|> parseReport
  <|> parseQuit

-- | Parse a symbol and consume trailing whitespace.
symbol :: String -> ReadP String
symbol s = string s <* skipSpaces

parseInitials :: ReadP Initials
parseInitials = (,) <$> get <*> get

parseNew :: ReadP Command
parseNew = New <$> (symbol "new" *> notPunc)

parseOpen :: ReadP Command
parseOpen = Open <$> (symbol "open" *> notPunc)

parseShow :: ReadP Command
parseShow = Show <$ symbol "show"

parseAdd :: ReadP Command
parseAdd = Add <$> (symbol "+" *> parseEvent)

parseDel :: ReadP Command
parseDel = Delete <$> (symbol "delete" *> int)

parseTot :: ReadP Command
parseTot = Total <$ symbol "total"

parseRec :: ReadP Command
parseRec = Reconcile <$ symbol "reconcile"

parseHelp :: ReadP Command
parseHelp = Help <$ symbol "help"

parseReport :: ReadP Command
parseReport = Report <$> (symbol "report" *> notPunc)

parseQuit :: ReadP Command
parseQuit = Quit <$ symbol "quit"

-- | Convert an event string to an 'Event'.
parseEvent :: ReadP Event
parseEvent = do
  dt   <- parseDay <* optional punc <* skipSpaces
  desc <- (quote <* skipSpaces <|> notPunc <* sep)
  pyr  <- parseInitials <* optional punc <* skipSomeSpace
  amt  <- parseRational <* optional punc <* skipSomeSpace
  ps   <- parseParticipants
  return $ Event 0 desc dt pyr ps amt

-- | Parse a Date.
parseDay :: ReadP Day
parseDay = parseTimeM True defaultTimeLocale "%-m/%-d/%-y"
       =<< munch1 (\c -> c /= ',' && c /= ' ')

-- | Parse a rational number.
parseRational :: ReadP Rational
parseRational = (/ 100) . toRational . (* 100) <$> double

-- | Parse a non-empty list of 'Initials'.
parseParticipants :: ReadP (NonEmpty Initials)
parseParticipants = N.fromList <$> sepBy1 parseInitials (char ' ') <* eof

isPunc :: Char -> Bool
isPunc = (`elem` ",.;:")

-- | Parse  punctuation.
punc :: ReadP Char
punc =  satisfy isPunc

quote :: ReadP String
quote = between (char '"') (char '"') (many1 get)

-- | Parse a forward slash.
slash :: ReadP Char
slash = char '/'

-- | Parse until reaching a punctuation mark.
notPunc :: ReadP String
notPunc = munch1 (not . isPunc)

-- | Separator parser.
sep :: ReadP Char
sep = punc <* skipSpaces

skipSomeSpace ::ReadP String
skipSomeSpace = munch1 (== ' ')

-- | Parse an Int.
int :: ReadP Int
int = readS_to_P reads

-- | Parse a Double.
double :: ReadP Double
double = readS_to_P reads
