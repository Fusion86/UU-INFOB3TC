module Calendar where

import Control.Monad (foldM, liftM2, replicateM)
import Data.Functor (($>), (<&>))
import Data.List (intercalate, sort)
import DateTime
import Debug.Trace (trace)
import ParseLib.Abstract
import System.IO (IOMode (ReadMode), hGetContents, openFile)
import Prelude hiding (sequence, ($>), (*>), (<$), (<*))

-- Exercise 6
data Calendar = Calendar
  { properties :: [Token],
    events :: [Event]
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
data Token
  = BeginToken String
  | VersionToken
  | ProdIdToken String
  | UidToken String
  | DtStampToken DateTime
  | DtStartToken DateTime
  | DtEndToken DateTime
  | SummaryToken String
  | DescriptionToken String
  | LocationToken String
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
        <|> parseKeyStringToken DescriptionToken "DESCRIPTION"
        <|> parseKeyStringToken LocationToken "LOCATION"
        <|> parseKeyStringToken EndToken "END"

    parseKeyWithValue :: String -> Parser Char s -> Parser Char s
    parseKeyWithValue key valueParser =
      token key *> symbol ':' *> valueParser <* token "\r\n"
    -- or
    -- token key *> symbol ':' *> valueParser <* optional (symbol '\r') <* symbol '\n'

    parseKeyStringToken :: (String -> Token) -> String -> Parser Char Token
    parseKeyStringToken ctor key = parseKeyWithValue key (greedy notNewline) <&> ctor

    parseKeyDateTimeToken :: (DateTime -> Token) -> String -> Parser Char Token
    parseKeyDateTimeToken ctor key = parseKeyWithValue key parseDateTime <&> ctor

    parseKeyVersionToken :: Parser Char Token
    parseKeyVersionToken = parseKeyWithValue "VERSION" (token "2.0") $> VersionToken

parseCalendar :: Parser Token Calendar
parseCalendar = do
  symbol (BeginToken "VCALENDAR")
  potProps <- replicateM 2 anySymbol
  props <- maybe failp return (constructProps (sort potProps))
  events <- greedy parseEvent
  symbol (EndToken "VCALENDAR")
  return $ Calendar props events
  where
    constructProps :: [Token] -> Maybe [Token]
    constructProps xs@[VersionToken, ProdIdToken _] = Just xs
    constructProps _ = Nothing

    parseEvent :: Parser Token Event
    parseEvent = do
      symbol beginToken
      props <- many (satisfy (/= endToken))
      symbol endToken
      maybe failp return (constructEvent (sort props))
      where
        beginToken = BeginToken "VEVENT"
        endToken = EndToken "VEVENT"

    constructEvent :: [Token] -> Maybe Event
    constructEvent ((UidToken uid) : (DtStampToken dtstamp) : (DtStartToken dtstart) : (DtEndToken dtend) : opts) =
      foldM f (Event uid dtstamp dtstart dtend Nothing Nothing Nothing) opts
      where
        f :: Event -> Token -> Maybe Event
        f e@Event {summary = Nothing} (SummaryToken str) = Just e {summary = Just str}
        f e@Event {description = Nothing} (DescriptionToken str) = Just $ e {description = Just str}
        f e@Event {location = Nothing} (LocationToken str) = Just $ e {location = Just str}
        f e _ = Nothing
    constructEvent _ = Nothing

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
    printProp VersionToken = "VERSION:2.0"
    printProp (ProdIdToken val) = "PRODID:" ++ val
    printProp _ = error "Not a property"

    printEvent :: Event -> String
    printEvent (Event uid stamp start end sum desc loc) =
      intercalate
        "\r\n"
        ( [ "BEGIN:VEVENT",
            "UID:" ++ uid,
            "DTSTAMP:" ++ printDateTime stamp,
            "DTSTART:" ++ printDateTime start,
            "DTEND:" ++ printDateTime end
          ]
            ++ ["SUMMARY:" ++ x | Just x <- [sum]]
            ++ ["DESCRIPTION:" ++ x | Just x <- [desc]]
            ++ ["LOCATION:" ++ x | Just x <- [loc]]
            ++ ["END:VEVENT"]
        )
