module Calendar where

import DateTime
import ParseLib.Abstract
import Prelude hiding (sequence, ($>), (*>), (<$), (<*))

-- Exercise 6
data Calendar = Calendar
  { properties :: [CalendarProp],
    event :: [Event]
  }
  deriving (Eq, Ord)

data Event
  = DateTimeStamp DateTime
  | Uid String
  | DateTimeStart DateTime
  | DateTimeEnd DateTime
  | Description String
  | Summary String
  | Location String
  deriving (Eq, Ord)

data CalendarProp
  = ProdId String
  | Version
  deriving (Eq, Ord)

-- Exercise 7
data Token = Token
  { key :: String,
    value :: String
  }
  deriving (Eq, Ord, Show)

scanCalendar :: Parser Char [Token]
scanCalendar = greedy parseToken
  where
    notNewline = satisfy (\c -> c /= '\r' && c /= '\n') 

    parseToken :: Parser Char Token
    parseToken = do
      key <- greedy $ satisfy (/= ':')
      symbol ':'
      value <- greedy notNewline
      optional $ symbol '\r'
      symbol '\n'
      return $ Token key value

parseCalendar :: Parser Token Calendar
parseCalendar = undefined

recognizeCalendar :: String -> Maybe Calendar
recognizeCalendar s = run scanCalendar s >>= run parseCalendar

-- Exercise 8
readCalendar :: FilePath -> IO (Maybe Calendar)
readCalendar = undefined

-- Exercise 9
-- DO NOT use a derived Show instance. Your printing style needs to be nicer than that :)
printCalendar :: Calendar -> String
printCalendar = undefined
