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
  = StringToken String String
  | DateTimeToken String DateTime
  deriving (Eq, Ord)

scanCalendar :: Parser Char [Token]
scanCalendar = greedy parseToken
  where
    notNewline = satisfy (\c -> c /= '\r' && c /= '\n')

    parseToken :: Parser Char Token
    parseToken = parseKeyStringToken <|> parseKeyDateTimeToken

    parseKeyWithValue :: Parser Char s -> Parser Char (String, s)
    parseKeyWithValue valueParser = do
      key <- greedy $ satisfy (/= ':')
      symbol ':'
      value <- valueParser
      optional (symbol '\r')
      symbol '\n'
      return (key, value)

    parseKeyStringToken :: Parser Char Token
    parseKeyStringToken = parseKeyWithValue (greedy notNewline) <&> uncurry StringToken

    parseKeyDateTimeToken :: Parser Char Token
    parseKeyDateTimeToken = parseKeyWithValue parseDateTime <&> uncurry DateTimeToken

parseCalendar :: Parser Token Calendar
parseCalendar = do
  symbol (StringToken "BEGIN" "VCALENDAR")
  symbol (StringToken "VERSION" "2.0")
  prodid <- requireKey "PRODID"
  symbol (StringToken "BEGIN" "VEVENT")
  return $ Calendar [] []
  where
    requireKey :: String -> Parser Token Token
    requireKey key = satisfy (requireKey' key)
      where
        requireKey' expected (StringToken actual _) = expected == actual
        requireKey' expected (DateTimeToken actual _) = expected == actual

recognizeCalendar :: String -> Maybe Calendar
recognizeCalendar s = run scanCalendar s >>= run parseCalendar

-- Exercise 8
readCalendar :: FilePath -> IO (Maybe Calendar)
readCalendar fp = readFile fp <&> recognizeCalendar

-- Exercise 9
-- DO NOT use a derived Show instance. Your printing style needs to be nicer than that :)
printCalendar :: Calendar -> String
printCalendar = undefined
