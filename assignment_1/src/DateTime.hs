module DateTime where

import Data.Char
import Data.Functor ((<&>))
import Data.Maybe
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
  sequence (replicate count parseDigit) <&> ctor . digitsToNumber

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
parsePrint s = fmap printDateTime $ run parseDateTime s

-- Exercise 5
checkDateTime :: DateTime -> Bool
checkDateTime (DateTime (Date y m d) (Time hh mm ss) _) =
  validRange 1 12 (runMonth m)&& (validDay (runYear y) (runMonth m) (runDay d)) && validRange  0 23 (runHour hh)&& validRange  0 59 (runMinute mm)&& validRange  0 59 (runSecond ss)
  where
    validRange min max actual = min <= actual && actual <= max
    validDay y m d = validRange 0 (daysInMonth y m d) d
    daysInMonth y m d
      | m == 2 = if isLeapYear y then 29 else 28
      | m `elem` [1, 3, 5, 7, 8, 10, 12] = 31
      | otherwise = 30
    isLeapYear y = divBy 400 || (divBy 4 && not (divBy 100))
      where
        divBy x = mod y x == 0

parseCheck s = checkDateTime <$> run parseDateTime s
