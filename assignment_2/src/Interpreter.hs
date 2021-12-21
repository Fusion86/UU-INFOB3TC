module Interpreter where

import Algebra
import Control.Monad (replicateM)
import Data.Char (isSpace)
import Data.List (intercalate)
import Data.List.Split (chunksOf)
import Data.Map (Map, toAscList)
import qualified Data.Map as L
import Lexer
import Model
import ParseLib.Abstract
import Parser
import Prelude hiding ((<$), (<*))

data Contents = Empty | Lambda | Debris | Asteroid | Boundary deriving (Show, Eq, Ord)

type Size = Int

type Pos = (Int, Int)

type Space = Map Pos Contents

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

    dims :: (Int, Int)
    dims = foldr (f . fst) (0, 0) lst
      where
        f :: (Int, Int) -> (Int, Int) -> (Int, Int)
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
type Ident = ()

type Commands = ()

type Heading = ()

type Environment = Map Ident Commands

type Stack = Commands

data ArrowState = ArrowState Space Pos Heading Stack

data Step
  = Done Space Pos Heading
  | Ok ArrowState
  | Fail String

-- | Exercise 8
toEnvironment :: String -> Environment
toEnvironment = undefined

-- | Exercise 9
step :: Environment -> ArrowState -> Step
step = undefined
