module Main where

import Common
import Control.Monad (when)
import Data.Char
import Data.Foldable
import Data.Functor ((<&>))
import Data.List (isSuffixOf)
import qualified Data.Map as L
import Data.Maybe (listToMaybe)
import Driver
import Interpreter
import ParseLib.Abstract
import System.Directory (getDirectoryContents)

main :: IO ()
main = do
  examples <- getDirectoryContents "./examples"
  let progs = filter (".arrow" `isSuffixOf`) examples
      fullProgs = map ("./examples/" ++) progs
      spaces = filter (".space" `isSuffixOf`) examples
      fullSpaces = map ("./examples/" ++) spaces

  print "Programs:"
  progIdx <- pickFromList progs
  src <- readFile (fullProgs !! progIdx)
  let env = toEnvironment src

  case L.lookup "start" env of
    Nothing -> print "Program does not have a start function. Exiting..."
    Just startFunc -> do
      print "Spaces:"
      spaceIdx <- pickFromList spaces
      spaceStr <- readFile (fullSpaces !! spaceIdx)

      case run parseSpace spaceStr of
        Nothing -> print "Could't parse space. Exiting..."
        Just space -> do
          pos <- askStartCoords

          print "Starting direction:"
          dirIdx <- pickFromList heading
          let dir = parseHeading (heading !! dirIdx)
              state = ArrowState space pos dir startFunc

          print "Initial state:"
          printArrowState state

          print "Mode:"
          mode <- pickFromList ["Interactive", "Batch"]

          if mode == 0
            then do
              interactive env state
            else do
              let (space, pos, heading) = batch env state
              print "Final state:"
              printArrowState (ArrowState space pos heading [])
  return ()
  where
    heading = ["North", "East", "South", "West"]

    -- Returns the index of the picked item.
    pickFromList :: [String] -> IO Int
    pickFromList lst = do
      -- glhf
      foldlM (\b a -> print (show b ++ ". " ++ a) >> return (b + 1)) 1 lst
      putStr "Choice: "
      x <- getLine
      let nr = read x :: Int
      if nr > 0 && nr <= length lst
        then return $ nr - 1
        else do
          print "Invalid choice, try again..."
          pickFromList lst

    parseHeading :: String -> Heading
    parseHeading "North" = North
    parseHeading "East" = East
    parseHeading "South" = South
    parseHeading "West" = West
    parseHeading x = error $ "Can't parse heading from '" ++ x ++ "'"

    -- Do notation is better, change my mind.
    askStartCoords :: IO Pos
    askStartCoords =
      putStr "Enter starting coordinates y,x: "
        >> getLine
          >>= maybe askStartCoords return . run parseCoords

    run :: Parser a b -> [a] -> Maybe b
    run p d = f (parse p d)
      where
        f [a] = Just (fst a)
        f _ = Nothing

    parseCoords :: Parser Char Pos
    parseCoords = do
      x <- parseCoord
      symbol ','
      y <- parseCoord
      return (x, y)

    parseCoord :: Parser Char Int
    parseCoord = do
      nrs <- greedy parseDigit
      return $ digitsToNumber nrs
      where
        parseDigit :: Parser Char Int
        parseDigit = do
          dig <- satisfy isDigit
          return $ digitToInt dig

        digitsToNumber :: [Int] -> Int
        digitsToNumber = foldl (\a b -> 10 * a + b) 0
