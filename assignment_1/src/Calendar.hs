module Calendar where

import Control.Monad (liftM2)
import Data.Functor (($>), (<&>))
import Data.List (intercalate)
import DateTime
import ParseLib.Abstract
import System.IO (IOMode (ReadMode), hGetContents, openFile)
import Prelude hiding (sequence, ($>), (*>), (<$), (<*))

-- Exercise 6
data Calendar = Calendar
  { properties :: [Token],
    event :: [Event]
  }
  deriving (Eq, Ord)

data Event = Event
  { uid :: String,
    dateTimeStamp :: DateTime,
    dateTimeStart :: DateTime,
    dateTimeEnd :: DateTime,
    summary :: String
  }
  deriving (Eq, Ord)

-- Exercise 7
data Token
  = StringToken {key :: String, strValue :: String}
  | DateTimeToken {key :: String, dtValue :: DateTime}
  | VersionToken
  deriving (Eq, Ord)

scanCalendar :: Parser Char [Token]
scanCalendar = greedy parseToken
  where
    notNewline = satisfy (\c -> c /= '\r' && c /= '\n')

    parseToken :: Parser Char Token
    parseToken = parseKeyDateTimeToken <|> parseKeyStringToken

    parseKeyWithValue :: Parser Char s -> Parser Char (String, s)
    parseKeyWithValue valueParser = do
      key <- greedy $ satisfy (/= ':')
      symbol ':'
      value <- valueParser
      token "\r\n"
      return (key, value)

    -- You can't tell me that this is better code.
    -- parseKeyWithValue :: Parser Char s -> Parser Char (String, s)
    -- parseKeyWithValue valueParser = liftM2 (,) (greedy (satisfy (/= ':')) <* symbol ':') (valueParser <* optional (symbol '\r') <* symbol '\n')

    parseKeyStringToken :: Parser Char Token
    parseKeyStringToken = parseKeyWithValue (greedy notNewline) <&> uncurry StringToken

    parseKeyDateTimeToken :: Parser Char Token
    parseKeyDateTimeToken = parseKeyWithValue parseDateTime <&> uncurry DateTimeToken

parseCalendar :: Parser Token Calendar
parseCalendar = do
  symbol (StringToken "BEGIN" "VCALENDAR")
  symbol (StringToken "VERSION" "2.0")
  prodid <- requireString "PRODID"
  symbol (StringToken "BEGIN" "VEVENT")
  uid <- requireString "UID"
  stamp <- requireDateTime "DTSTAMP"
  start <- requireDateTime "DTSTART"
  end <- requireDateTime "DTEND"
  summary <- requireString "SUMMARY"
  symbol (StringToken "END" "VEVENT")
  symbol (StringToken "END" "VCALENDAR")
  return $ Calendar [VersionToken, StringToken "PRODID" prodid] [Event uid stamp start end summary]
  where
    requireString :: String -> Parser Token String
    requireString key = satisfy (\(StringToken k v) -> key == k) <&> strValue

    -- Why do we have to use a helper function here whereas a lambda is sufficient in the above case???
    requireDateTime :: String -> Parser Token DateTime
    requireDateTime key = satisfy f <&> dtValue
      where
        f (DateTimeToken k v) = key == k
        f _ = False

recognizeCalendar :: String -> Maybe Calendar
recognizeCalendar s = run scanCalendar s >>= run parseCalendar

-- Exercise 8
-- We don't call `hSetNewlineMode` because this is already called in the main function.
readCalendar :: FilePath -> IO (Maybe Calendar)
readCalendar fp = openFile fp ReadMode >>= hGetContents <&> recognizeCalendar

-- Why not just?
-- readCalendar fp = readFile fp <&> recognizeCalendar

-- Exercise 9
-- DO NOT use a derived Show instance. Your printing style needs to be nicer than that :)
printCalendar :: Calendar -> String
printCalendar (Calendar props events) =
  intercalate
    "\r\n"
    ( ["BEGIN:VCALENDAR"]
        ++ map printProp props
        ++ map printEvent events
        ++ ["END:VCALENDAR\r\n"] -- Include trailing newline
    )
  where
    printProp :: Token -> String
    printProp (StringToken k v) = k ++ ":" ++ v
    printProp (DateTimeToken k v) = k ++ ":" ++ printDateTime v
    printProp VersionToken = "VERSION:2.0"

    printEvent :: Event -> String
    printEvent (Event uid stamp start end sum) =
      intercalate
        "\r\n"
        [ "BEGIN:VEVENT",
          "UID:" ++ uid,
          "DTSTAMP:" ++ printDateTime stamp,
          "DTSTART:" ++ printDateTime start,
          "DTEND:" ++ printDateTime end,
          "SUMMARY:" ++ sum,
          "END:VEVENT"
        ]
