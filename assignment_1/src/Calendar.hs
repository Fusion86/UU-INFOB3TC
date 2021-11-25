module Calendar where

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

    -- parseToken :: Parser Char Token
    -- parseToken = do
    --   key <- greedy $ satisfy (/= ':')
    --   symbol ':'
    --   value <- greedy notNewline
    --   optional $ symbol '\r'
    --   symbol '\n'
    --   return $ Token key value

    parseToken :: Parser Char Token
    parseToken = undefined

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
