module Calendar where

import Control.Monad (liftM2, replicateM)
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
  props <- replicateM 2 (satisfy (stringToken ProdIdToken) <|> symbol VersionToken)
  events <- greedy parseEvent
  symbol (EndToken "VCALENDAR")
  return $ Calendar props events
  where
    parseEvent :: Parser Token Event
    parseEvent = do
      symbol beginToken
      props <- many (satisfy (/= endToken))
      symbol endToken

      let sorted = sort props
      let event = parse parseEventProps sorted

      case event of
        [] -> failp
        (e, _) : _ -> return e
      where
        beginToken = BeginToken "VEVENT"
        endToken = EndToken "VEVENT"

    parseEventProps :: Parser Token Event
    parseEventProps = do
      uid <- satisfyStr UidToken
      dtstamp <- satisfyDt DtStampToken
      dtstart <- satisfyDt DtStartToken
      dtend <- satisfyDt DtEndToken
      summary <- optional (satisfyStr SummaryToken)
      desc <- optional (satisfyStr DescriptionToken)
      loc <- optional (satisfyStr LocationToken)
      return $ Event uid dtstamp dtstart dtend summary desc loc

    satisfyStr :: (String -> Token) -> Parser Token String
    satisfyStr t = do
      x <- anySymbol
      if stringToken t x
        then return $ getStrVal x
        else failp

    satisfyDt :: (DateTime -> Token) -> Parser Token DateTime
    satisfyDt t = do
      x <- anySymbol
      if dateTimeToken t x
        then return $ getDtVal x
        else failp

    nullDateTime = DateTime (Date (Year 1970) (Month 1) (Day 1)) (Time (Hour 0) (Minute 0) (Second 0)) True

    stringToken :: (String -> Token) -> Token -> Bool
    stringToken ctor = typeMatch $ ctor ""

    dateTimeToken :: (DateTime -> Token) -> Token -> Bool
    dateTimeToken ctor = typeMatch $ ctor nullDateTime

    typeMatch :: Token -> Token -> Bool
    typeMatch (EndToken _) (EndToken _) = True
    typeMatch (UidToken _) (UidToken _) = True
    typeMatch (BeginToken _) (BeginToken _) = True
    typeMatch (DtEndToken _) (DtEndToken _) = True
    typeMatch (ProdIdToken _) (ProdIdToken _) = True
    typeMatch (DtStampToken _) (DtStampToken _) = True
    typeMatch (DtStartToken _) (DtStartToken _) = True
    typeMatch (SummaryToken _) (SummaryToken _) = True
    typeMatch (DescriptionToken _) (DescriptionToken _) = True
    typeMatch (LocationToken _) (LocationToken _) = True
    typeMatch _ _ = False

    getStrVal :: Token -> String
    getStrVal (EndToken v) = v
    getStrVal (UidToken v) = v
    getStrVal (BeginToken v) = v
    getStrVal (ProdIdToken v) = v
    getStrVal (SummaryToken v) = v
    getStrVal (DescriptionToken v) = v
    getStrVal (LocationToken v) = v
    getStrVal _ = error "Token value is not a string"

    getDtVal :: Token -> DateTime
    getDtVal (DtStampToken v) = v
    getDtVal (DtStartToken v) = v
    getDtVal (DtEndToken v) = v
    getDtVal _ = error "Token value is not a DateTime"

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
