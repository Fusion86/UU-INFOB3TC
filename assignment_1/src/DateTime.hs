module DateTime where

import Control.Monad (replicateM)
import Data.Char (Char, digitToInt, isDigit)
import Data.Functor ((<&>))
import Data.Maybe (Maybe, isJust, listToMaybe)
import ParseLib.Abstract
import Text.Printf (printf)
import Prelude hiding (sequence, ($>), (*>), (<$), (<*))

-- | "Target" datatype for the DateTime parser, i.e, the parser should produce elements of this type.
data DateTime = DateTime
  { date :: Date,
    time :: Time,
    utc :: Bool
  }
  deriving (Eq, Ord)

data Date = Date
  { year :: Year,
    month :: Month,
    day :: Day
  }
  deriving (Eq, Ord)

newtype Year = Year {runYear :: Int} deriving (Eq, Ord)

newtype Month = Month {runMonth :: Int} deriving (Eq, Ord)

newtype Day = Day {runDay :: Int} deriving (Eq, Ord)

data Time = Time
  { hour :: Hour,
    minute :: Minute,
    second :: Second
  }
  deriving (Eq, Ord)

newtype Hour = Hour {runHour :: Int} deriving (Eq, Ord)

newtype Minute = Minute {runMinute :: Int} deriving (Eq, Ord)

newtype Second = Second {runSecond :: Int} deriving (Eq, Ord)

-- Exercise 1
parseDateTime :: Parser Char DateTime
parseDateTime = do
  y <- parseDigits Year 4
  m <- parseDigits Month 2
  d <- parseDigits Day 2
  symbol 'T'
  hh <- parseDigits Hour 2
  mm <- parseDigits Minute 2
  ss <- parseDigits Second 2
  utc <- optional (symbol 'Z')
  return $ DateTime (Date y m d) (Time hh mm ss) (isJust utc)

parseDigits :: (Int -> b) -> Int -> Parser Char b
parseDigits ctor count = do
  replicateM count parseDigit <&> ctor . digitsToNumber

parseDigit :: Parser Char Int
parseDigit = do
  dig <- satisfy isDigit
  return $ digitToInt dig

digitsToNumber :: [Int] -> Int
digitsToNumber = foldl (\a b -> 10 * a + b) 0

-- Exercise 2
run :: Parser a b -> [a] -> Maybe b
run p d = listToMaybe (parse p d) <&> fst

-- Exercise 3
printDateTime :: DateTime -> String
printDateTime (DateTime (Date y m d) (Time hh mm ss) utc) =
  printf "%04d%02d%02dT%02d%02d%02d%s" (runYear y) (runMonth m) (runDay d) (runHour hh) (runMinute mm) (runSecond ss) (if utc then "Z" else "")

-- Exercise 4
parsePrint :: [Char] -> Maybe String
parsePrint s = printDateTime <$> run parseDateTime s

-- Exercise 5
checkDateTime :: DateTime -> Bool
checkDateTime (DateTime (Date y m d) (Time hh mm ss) _) =
  validRange 1 12 (runMonth m)
    && validDay (runYear y) (runMonth m) (runDay d)
    && validRange 0 23 (runHour hh)
    && validRange 0 59 (runMinute mm)
    && validRange 0 59 (runSecond ss)
  where
    validRange min max actual = min <= actual && actual <= max
    validDay y m d = validRange 0 (daysInMonth y m) d

daysInMonth :: Int -> Int -> Int
daysInMonth y m
  | m == 2 = if isLeapYear y then 29 else 28
  | m `elem` [1, 3, 5, 7, 8, 10, 12] = 31
  | otherwise = 30

isLeapYear :: Int -> Bool
isLeapYear y = divBy 400 || (divBy 4 && not (divBy 100))
  where
    divBy x = mod y x == 0

parseCheck :: [Char] -> Maybe Bool
parseCheck s = checkDateTime <$> run parseDateTime s

-- Might be incorrect, but we only need a relative difference so it doesn't really matter.
dateTimeToTimestamp :: DateTime -> Int
dateTimeToTimestamp
  (DateTime (Date (Year y) (Month m) (Day d)) (Time (Hour hh) (Minute mm) (Second ss)) _)
    | y > 1970 = seconds
    | otherwise = error "time did not exist before 1970"
    where
      yearDays = sum $ map (\x -> if isLeapYear x then 366 else 365) [1970 .. y]
      monthDays = sum $ map (daysInMonth y) [1 .. m]
      days = yearDays + monthDays + d
      hours = days * 24 + hh
      minutes = hours * 60 + mm
      seconds = minutes * 60 + ss

-- 0 = Monday ... 6 = Sunday
-- Function we came up with.
dayOfWeek' :: Int -> Int -> Int -> Int
dayOfWeek' y m d
  | y > 1970 = (days+nullDate) `mod` 7
  | otherwise = error "time did not exist before 1970"
  where
    yearDays = sum $ map (\x -> if isLeapYear x then 366 else 365) [1970 .. (y -1)]
    monthDays = sum $ map (daysInMonth y) [1 .. (m -1)]
    days = yearDays + monthDays + d
    nullDate = 2

-- Sakamoto's methods
-- Taken from https://en.wikipedia.org/wiki/Determination_of_the_day_of_the_week
dayOfWeek :: Int -> Int -> Int -> Int
dayOfWeek y' m d = (y + y `div` 4 - y `div` 100 + y `div` 400 + (t !! (m - 1)) + d) `mod` 7
  where
    t = [0, 3, 2, 5, 0, 3, 5, 1, 4, 6, 2, 4]
    y
      | m < 4 = y' - 1
      | otherwise = y'

monthToStr :: Int -> String
monthToStr 1 = "January"
monthToStr 2 = "February"
monthToStr 3 = "March"
monthToStr 4 = "April"
monthToStr 5 = "May"
monthToStr 6 = "June"
monthToStr 7 = "July"
monthToStr 8 = "Augustus"
monthToStr 9 = "September"
monthToStr 10 = "October"
monthToStr 11 = "November"
monthToStr 12 = "December"
monthToStr _ = "Invalid month"

-- 0 = Monday ... 6 = Sunday
dowToStr :: Int -> String
dowToStr 0 = "Mon"
dowToStr 1 = "Tue"
dowToStr 2 = "Wed"
dowToStr 3 = "Thu"
dowToStr 4 = "Fri"
dowToStr 5 = "Sat"
dowToStr 6 = "Sun"
dowToStr _ = "Invalid day of week"
