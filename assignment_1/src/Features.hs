module Features where

import Calendar
import Data.List (isInfixOf)
import Data.List.Split
import Data.Maybe (fromJust)
import DateTime
import Text.PrettyPrint.Boxes
import Text.Printf (printf)
import Prelude hiding ((<>))

-- Exercise 10
countEvents :: Calendar -> Int
countEvents = length . events

findEvents :: DateTime -> Calendar -> [Event]
findEvents dt = filter f . events
  where
    f e = dt >= dateTimeStart e && dt < dateTimeEnd e

checkOverlapping :: Calendar -> Bool
checkOverlapping = check . events
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
    summaryFilter events = [e | e <- events, Just sum <- [summary e], str `isInfixOf` sum]
    -- summaryFilter = filter (f . summary)
    --   where
    --     f x
    --       | Just x <- x = str `isInfixOf` x
    --       | otherwise = False

    getTimeSpent :: Event -> Int
    getTimeSpent e = diffMin (dateTimeStart e) (dateTimeEnd e)
      where
        diffMin :: DateTime -> DateTime -> Int
        diffMin dt1 dt2 = (dateTimeToTimestamp dt2 - dateTimeToTimestamp dt1) `div` 60

-- Exercise 11
ppMonth :: Year -> Month -> Calendar -> String
-- ppMonth y m cal = concatMap ((++) "\n" . show . summary) evs
-- ppMonth y m cal = concatMap printDay [1 .. days]
ppMonth y m cal = render $ vcat center1 [header, punctuateV left sepV $ map rowForDays weekChunks]
  where
    eventsToShow = 4
    dayWidth = 24

    -- TODO: Align calendar to always start on monday, it is a lot of work though.
    weekChunks = chunksOf 7 [1 .. days]

    header = alignVert center1 3 $ text (monthToStr (runMonth m) ++ " " ++ show (runYear y))

    rowForDays :: [Int] -> Box
    rowForDays days = punctuateH left sepH $ map boxDay days

    boxDay :: Int -> Box
    boxDay d = alignHoriz center1 dayWidth (alignHoriz left (dayWidth - 2) (headerBox // contentBox))
      where
        headerBox = text (show d ++ " " ++ dayStr)
        dayStr = dowToStr (dayOfWeek' (runYear y) (runMonth m) d)
        contentBox =
          alignVert top eventsToShow $
            vcat left (map boxEvent eventsToDisplay ++ footer)
        events = eventsOnDay d
        eventsToDisplay = take eventDisplayCount events
        eventDisplayCount
          | showDisplayMore = eventsToShow - 1
          | otherwise = eventsToShow
        showDisplayMore = length events > eventsToShow
        footer
          | showDisplayMore = [text $ "+ " ++ show (length events - eventDisplayCount) ++ " more"]
          | otherwise = []

    boxEvent :: Event -> Box
    -- boxEvent e = text (eventName ++ " @ " ++ formatDt (dateTimeStart e))
    boxEvent e = hsep 2 center1 [alignHoriz left nameFieldSize (text eventName), text (formatDt (dateTimeStart e))]
      where
        paddingSize = 2
        timeFieldSize = 7
        nameFieldSize = dayWidth - timeFieldSize - paddingSize
        eventName
          | Just x <- summary e =
            if length x > nameFieldSize
              then take (nameFieldSize - 1) x ++ "…"
              else x
          | otherwise = "Unnamed event"

    formatDt :: DateTime -> String
    formatDt dt = printf "%02d:%02d" h m
      where
        h = runHour . hour . time $ dt
        m = runMinute . minute . time $ dt

    sepH = vcat left $ replicate (eventsToShow + 1) $ char '|'
    sepV = text $ concat $ sep : replicate 6 ('+' : sep)
      where
        sep = replicate dayWidth '-'

    days = daysInMonth (runYear y) (runMonth m)
    -- evs = filter f (events cal)
    --   where
    --     start = DateTime (Date y m (Day 0)) (Time (Hour 0) (Minute 0) (Second 0)) True
    --     end = DateTime (Date y m (Day 32)) (Time (Hour 0) (Minute 0) (Second 0)) True
    --     f e = dateTimeStart e > start && dateTimeStart e < end

    eventsOnDay :: Int -> [Event]
    eventsOnDay day = filter f (events cal)
      where
        start = DateTime (Date y m (Day day)) (Time (Hour 0) (Minute 0) (Second 0)) True
        end = DateTime (Date y m (Day day)) (Time (Hour 24) (Minute 0) (Second 0)) True
        f e = dateTimeStart e >= start && dateTimeStart e <= end
