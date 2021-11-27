module Calendar where

import Data.Functor (($>), (<&>))
import DateTime
import ParseLib.Abstract
import Prelude hiding (sequence, ($>), (*>), (<$), (<*))

-- Exercise 6
data Calendar = Calendar
  { properties :: [Token],
    event :: [Event]
  }
  deriving (Eq, Ord)

data Event = Event
  {
  }
  deriving (Eq, Ord)

-- Exercise 7
data Token
  = BeginToken String
  | VersionToken
  | ProdIdToken String
  | UidToken String
  | DtStampToken DateTime
  | DtStartToken DateTime
  | DtEndToken DateTime
  | SummaryToken String
  | EndToken String
  deriving (Eq, Ord)

scanCalendar :: Parser Char [Token]
scanCalendar = greedy parseToken
  where
    notNewline = satisfy (\c -> c /= '\r' && c /= '\n')

    parseToken :: Parser Char Token
    parseToken =
      do
        parseKeyStringToken BeginToken "BEGIN"
        <|> parseKeyVersionToken
        <|> parseKeyStringToken ProdIdToken "PRODID"
        <|> parseKeyStringToken UidToken "UID"
        <|> parseKeyDateTimeToken DtStampToken "DTSTAMP"
        <|> parseKeyDateTimeToken DtStartToken "DTSTART"
        <|> parseKeyDateTimeToken DtEndToken "DTEND"
        <|> parseKeyStringToken SummaryToken "SUMMARY"
        <|> parseKeyStringToken EndToken "END"

    parseKeyWithValue :: String -> Parser Char s -> Parser Char s
    parseKeyWithValue key valueParser =
      token key *> symbol ':' *> valueParser <* optional (symbol '\r') <* symbol '\n'

    parseKeyStringToken :: (String -> Token) -> String -> Parser Char Token
    parseKeyStringToken ctor key = parseKeyWithValue key (greedy notNewline) <&> ctor

    parseKeyDateTimeToken :: (DateTime -> Token) -> String -> Parser Char Token
    parseKeyDateTimeToken ctor key = parseKeyWithValue key parseDateTime <&> ctor

    parseKeyVersionToken :: Parser Char Token
    parseKeyVersionToken = parseKeyWithValue "VERSION" (token "2.0") $> VersionToken

parseCalendar :: Parser Token Calendar
parseCalendar = do
  symbol (BeginToken "VCALENDAR")
  symbol VersionToken
  prodid <- symbol (ProdIdToken "")
  return $ Calendar [] []

recognizeCalendar :: String -> Maybe Calendar
recognizeCalendar s = run scanCalendar s >>= run parseCalendar

-- Exercise 8
readCalendar :: FilePath -> IO (Maybe Calendar)
readCalendar = undefined

-- Exercise 9
-- DO NOT use a derived Show instance. Your printing style needs to be nicer than that :)
printCalendar :: Calendar -> String
printCalendar = undefined
