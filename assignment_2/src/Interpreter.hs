module Interpreter where

import Algebra
-- import Common
import Control.Monad (replicateM)
import Data.Char (isSpace)
import Data.List (intercalate)
import Data.List.Split (chunksOf)
import Data.Map (Map, adjust, toAscList, (!))
import qualified Data.Map as L
import Data.Maybe (fromMaybe)
import Lexer
import Model
import ParseLib.Abstract
import Parser
import Prelude hiding ((<$), (<*))

data Contents = Empty | Lambda | Debris | Asteroid | Boundary deriving (Show, Eq, Ord)

type Size = Int

type Pos = (Int, Int)

type Space = Map Pos Contents

-- Disable trace, but leave the code there so that we can easily toggle it.
trace _ a = a

-- | Parses a space file that can be found in the examples folder.
parseSpace :: Parser Char Space
parseSpace = do
  (mr, mc) <-
    parenthesised ((,) <$> natural <* symbol ',' <*> natural)
      <* spaces
  -- read |mr + 1| rows of |mc + 1| characters
  css <- replicateM (mr + 1) (replicateM (mc + 1) contents)
  -- convert from a list of lists to a finite map representation
  return $
    L.fromList $
      concat $
        zipWith
          ( \r cs ->
              zipWith (\c d -> ((r, c), d)) [0 ..] cs
          )
          [0 ..]
          css
  where
    spaces :: Parser Char String
    spaces = greedy (satisfy isSpace)

    contents :: Parser Char Contents
    contents =
      choice (Prelude.map (\(f, c) -> f <$ symbol c) contentsTable)
        <* spaces

-- | Conversion table
contentsTable :: [(Contents, Char)]
contentsTable =
  [ (Empty, '.'),
    (Lambda, '\\'),
    (Debris, '%'),
    (Asteroid, 'O'),
    (Boundary, '#')
  ]

-- Exercise 7
printSpace :: Space -> String
printSpace space =
  "(" ++ show (fst dims) ++ "," ++ show (snd dims) ++ ")\n"
    ++ intercalate "\n" (map printRow rows)
  where
    lst = toAscList space
    rows = chunksOf (snd dims + 1) lst

    dims :: Pos
    dims = foldr (f . fst) (0, 0) lst
      where
        f :: Pos -> Pos -> Pos
        f (a1, a2) (b1, b2) = (max a1 b1, max a2 b2)

    contentsMap = L.fromList contentsTable

    printRow :: [(Pos, Contents)] -> String
    printRow a = foldr f "" a
      where
        f :: (Pos, Contents) -> String -> String
        f (_, c) str = case L.lookup c contentsMap of
          Nothing -> error $ "contents '" ++ show c ++ "' does not exist in the contentsTable."
          Just a -> a : str

-- These three should be defined by you
type Ident = String

type Commands = [Command]

data Heading = North | East | South | West deriving (Show, Eq)

type Environment = Map Ident Commands

type Stack = Commands

data ArrowState = ArrowState
  { space :: Space,
    pos :: Pos,
    heading :: Heading,
    stack :: Stack
  }
  deriving (Show)

data Step
  = Done Space Pos Heading
  | Ok ArrowState
  | Fail String
  deriving (Show)

-- | Exercise 8
toEnvironment :: String -> Environment
toEnvironment str
  | checkProgram program = mapProgram program
  | otherwise = error "checkProgram failed."
  where
    program = parseProgram (alexScanTokens str)

    mapProgram :: Program -> Environment
    mapProgram (Program rules) = foldr f L.empty rules
      where
        f (Rule name cmds) b = L.insert name cmds b

-- | Exercise 9
step :: Environment -> ArrowState -> Step
step _ state@(ArrowState space pos heading []) = Done space pos heading
step env state@(ArrowState space pos@(y, x) heading (cmd : cmds)) = f cmd
  where
    f GoCommand = Ok $ newState {pos = newPos}
      where
        newPos
          | heading == North = tryMove (y - 1, x)
          | heading == East = tryMove (y, x + 1)
          | heading == South = tryMove (y + 1, x)
          | otherwise = tryMove (y, x - 1)
    f TakeCommand = Ok $ newState {space = adjust (const Empty) pos space}
    f MarkCommand = Ok $ newState {space = adjust (const Lambda) pos space}
    f NothingCommand = Ok newState
    f (TurnCommand dir) = Ok $ newState {heading = turnDirection dir}
    --
    f (CaseCommand dir' alts)
      | dir == North = matchPos (y - 1, x)
      | dir == East = matchPos (y, x + 1)
      | dir == South = matchPos (y + 1, x)
      | otherwise = matchPos (y, x - 1)
      where
        dir = turnDirection dir'

        -- Pattern match on a given position.
        matchPos :: Pos -> Step
        matchPos pos =
          trace ("Matching " ++ show pos ++ " for object '" ++ show obj ++ "'") $
            match obj alts
          where
            obj = getObj pos

        -- Recursive helper function for the matchPos function.
        match :: Contents -> [Alt] -> Step
        match obj [] = Fail $ "Pattern '" ++ show obj ++ "' not found in pattern match."
        match obj ((Alt pat patCmds) : alts)
          | pat == UnderscorePattern || obj == toContents pat =
            trace ("Contents '" ++ show obj ++ "' matches pattern '" ++ show pat ++ "'") $
              Ok $ state {stack = patCmds ++ cmds}
          | otherwise = trace "No match!" $ match obj alts
          where
            toContents :: Pattern -> Contents
            toContents EmptyPattern = Empty
            toContents LambdaPattern = Lambda
            toContents DebrisPattern = Debris
            toContents AsteroidPattern = Asteroid
            toContents BoundaryPattern = Boundary
            toContents UnderscorePattern = error "can't convert UnderscorePattern to Contents."
    --
    f (FunctionCall name) = case L.lookup name env of
      Nothing -> Fail $ "Command '" ++ name ++ "' not found."
      Just func -> trace ("Calling function: " ++ name) $ Ok $ state {stack = func ++ cmds}

    -- New state, with the top cmd removed from the stack.
    newState = state {stack = cmds}

    -- Get object at a given position, or boundary if it is outside the space.
    getObj :: Pos -> Contents
    getObj pos = fromMaybe Boundary (L.lookup pos space)

    -- Check if the given newPos is a valid position. Otherwise return the current position.
    tryMove p
      | obj == Empty = p
      | obj == Lambda = p
      | obj == Debris = p
      | otherwise = pos
      where
        obj = getObj p

    -- Turn in a given direction and return the new direction in which we are heading.
    turnDirection :: Direction -> Heading
    turnDirection dir
      | heading == North && dir == LeftDirection = West
      | heading == North && dir == RightDirection = East
      | heading == East && dir == LeftDirection = North
      | heading == East && dir == RightDirection = South
      | heading == South && dir == LeftDirection = East
      | heading == South && dir == RightDirection = West
      | heading == West && dir == LeftDirection = South
      | heading == West && dir == RightDirection = North
      | otherwise = heading
