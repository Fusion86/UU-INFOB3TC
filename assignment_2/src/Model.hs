module Model where

-- Exercise 1
data Token
  = ArrowToken
  | DotToken
  | CommaToken
  | GoToken
  | TakeToken
  | MarkToken
  | NothingToken
  | TurnToken
  | CaseToken
  | OfToken
  | EndToken
  | LeftToken
  | RightToken
  | FrontToken
  | SemiColonToken
  | EmptyToken
  | LambdaToken
  | DebrisToken
  | AsteroidToken
  | BoundaryToken
  | UnderscoreToken
  | IdentifierToken String
  deriving (Eq, Show)

-- Exercise 2
newtype Program = Program [Rule] deriving (Show)

data Rule = Rule String [Command] deriving (Show)

data Command
  = GoCommand
  | TakeCommand
  | MarkCommand
  | NothingCommand
  | TurnCommand Direction
  | CaseCommand Direction [Alt]
  | FunctionCall String
  deriving (Show)

data Direction = LeftDirection | RightDirection | FrontDirection deriving (Show, Eq)

data Alt = Alt Pattern [Command] deriving (Show)

data Pattern
  = EmptyPattern
  | LambdaPattern
  | DebrisPattern
  | AsteroidPattern
  | BoundaryPattern
  | UnderscorePattern
  deriving (Eq, Show)
