module Calendar where

import Control.Monad (liftM2, replicateM)
import Data.Functor (($>), (<&>))
import Data.List (find, intercalate)
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
    summary :: Maybe String,
    description :: Maybe String,
    location :: Maybe String
  }
  deriving (Eq, Ord)

-- Exercise 7
data Token = Token {key :: String, value :: String}
  deriving (Eq, Ord)

scanCalendar :: Parser Char [Token]
scanCalendar = greedy parseToken
  where
    notNewline = satisfy (\c -> c /= '\r' && c /= '\n')

    parseToken :: Parser Char Token
    parseToken = do
      key <- greedy $ satisfy (/= ':')
      symbol ':'
      value <- greedy notNewline
      token "\r\n" -- According to the spec CRLF is required.
      return (Token key value)

parseCalendar :: Parser Token Calendar
parseCalendar = do
  symbol (Token "BEGIN" "VCALENDAR")
  props <- replicateM 2 (requireKey "PRODID" <|> requireKey "VERSION")
  events <- greedy parseEvent
  symbol (Token "END" "VCALENDAR")
  return $ Calendar props events
  where
    requireKey :: String -> Parser Token Token
    requireKey k = satisfy $ compareKey k

    compareKey :: String -> Token -> Bool
    compareKey k token = key token == k

    parseEvent :: Parser Token Event
    parseEvent = do
      props <- pack beginEvent eventProps endEvent

      let uid = getRequiredProp "UID" props
      let stampStr = getRequiredProp "DTSTAMP" props
      let startStr = getRequiredProp "DTSTART" props
      let endStr = getRequiredProp "DTEND" props

      let stamp = run parseDateTime stampStr 

      return $ Event uid undefined undefined undefined Nothing Nothing Nothing
      where
        getRequiredProp :: String -> [Token] -> String
        getRequiredProp k tokens = case getProp k tokens of
          Just x -> x
          Nothing -> error "required property '" ++ k ++ "' is missing."

        getProp :: String -> [Token] -> Maybe String
        getProp key tokens =
          case find (compareKey key) tokens of
            Just x -> Just $ value x
            _ -> Nothing

        beginEvent = symbol (Token "BEGIN" "VEVENT")
        endEvent = symbol (Token "END" "VEVENT")

        eventProps =
          greedy $
            requireKey "UID"
              <|> requireKey "DTSTAMP"
              <|> requireKey "DTSTART"
              <|> requireKey "DTEND"
              <|> requireKey "SUMMARY"
              <|> requireKey "DESCRIPTION"
              <|> requireKey "LOCATION"

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
    printProp (Token k v) = k ++ ":" ++ v

    printEvent :: Event -> String
    printEvent (Event uid stamp start end sum desc loc) =
      intercalate
        "\r\n"
        [ "BEGIN:VEVENT",
          "UID:" ++ uid,
          "DTSTAMP:" ++ printDateTime stamp,
          "DTSTART:" ++ printDateTime start,
          "DTEND:" ++ printDateTime end,
          -- "SUMMARY:" ++ sum,
          "END:VEVENT"
        ]
