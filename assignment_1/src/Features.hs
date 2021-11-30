module Features where

import Calendar
import Data.List (isInfixOf)
import Data.Maybe (fromJust)
import DateTime
import Text.PrettyPrint.Boxes

-- Exercise 10
countEvents :: Calendar -> Int
countEvents = length . events

findEvents :: DateTime -> Calendar -> [Event]
findEvents dt = filter f . events
  where
    f e = dt >= dateTimeStart e && dt <= dateTimeEnd e

checkOverlapping :: Calendar -> Bool
checkOverlapping = check . events -- volgensmij kun je hier een fold knallen maar idk
  where
    check :: [Event] -> Bool
    check [] = False
    check [x] = False
    check (x : xs)
      | any (overlaps x) xs = True
      | otherwise = check xs

    overlaps :: Event -> Event -> Bool
    -- Two events overlap when one events start-point is in between the start and end-points of another event
    overlaps e1 e2 = (dts1 <= dts2 && dte1 > dts2) || (dts2 <= dts1 && dte2 > dts1)
      where
        dts1 = dateTimeStart e1
        dte1 = dateTimeEnd e1
        dts2 = dateTimeStart e2
        dte2 = dateTimeEnd e2

timeSpent :: String -> Calendar -> Int
timeSpent str = sum . map getTimeSpent . summaryFilter . events
  where
    summaryFilter :: [Event] -> [Event]
    -- summaryFilter events = [e | e <- events, Just sum <- [summary e], str `isInfixOf` sum]
    summaryFilter = filter (f . summary)
      where
        f x
          | Just x <- x = str `isInfixOf` x
          | otherwise = False

    getTimeSpent :: Event -> Int
    getTimeSpent e = diffMin (dateTimeStart e) (dateTimeEnd e)
      where
        -- Pretty code
        diffMin :: DateTime -> DateTime -> Int
        diffMin dt1 dt2 = (dateTimeToTimestamp dt2 - dateTimeToTimestamp dt1) `div` 60

-- Exercise 11
ppMonth :: Year -> Month -> Calendar -> String
ppMonth _ _ cal = printCalendar cal
