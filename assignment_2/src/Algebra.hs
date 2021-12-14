module Algebra where

import Model

-- Exercise 5
type Algebra r =
  ( [r] -> r, -- program
    String -> [r] -> r, -- rule
    r, -- go
    r, -- take command
    r, -- mark command
    r, -- nothing command
    r -> r, -- turn command
    r -> [r] -> r, -- case command
    String -> r, -- function call
    r, -- left dir
    r, -- right dir
    r, -- front dir
    r -> [r] -> r, -- alt
    r, -- empty pattern
    r, -- lambda pattern
    r, -- debris pattern
    r, -- asteroid pattern
    r, -- boundary pattern
    r -- underscore pattern
  )

fold :: Algebra r -> Program -> r
fold
  ( prog,
    rule,
    go,
    take,
    mark,
    nothing,
    turn,
    case',
    func,
    left,
    right,
    front,
    alt,
    empty,
    lambda,
    debris,
    asteroid,
    boundary,
    underscore
    ) = f
    where
      f (Program rules) = prog (map fr rules)
      fr (Rule ident rules) = rule ident (map fc rules)
      fc GoCommand = go
      fc TakeCommand = take
      fc MarkCommand = mark
      fc NothingCommand = nothing
      fc (TurnCommand dir) = turn (fd dir)
      fc (CaseCommand dir alts) = case' (fd dir) (map fa alts)
      fc (FunctionCall ident) = func ident
      fd LeftDirection = left
      fd RightDirection = right
      fd FrontDirection = front
      fa (Alt pat cmds) = alt (fp pat) (map fc cmds)
      fp EmptyPattern = empty
      fp LambdaPattern = lambda
      fp DebrisPattern = debris
      fp AsteroidPattern = asteroid
      fp BoundaryPattern = boundary
      fp UnderscorePattern = underscore

-- Exercise 6

checkProgram :: Program -> Bool
checkProgram = fold algebra
  where
    algebra :: Algebra String
    algebra =
      ( prog,
        const $ const True,
        True,
        True,
        True,
        True,
        const True,
        const $ const True,
        const True,
        True,
        True,
        True,
        const $ const True,
        True,
        True,
        True,
        True,
        True,
        True
      )
    prog :: [Bool] -> Bool
    prog = undefined
