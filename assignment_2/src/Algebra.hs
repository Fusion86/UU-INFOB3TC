module Algebra where

import Model

-- Exercise 5

type Algebra p r c d a pat =
  ( -- program
    [r] -> p, --

    -- rule
    String -> [c] -> r, --

    -- commands
    c, -- go
    c, -- take command
    c, -- mark command
    c, -- nothing command
    d -> c, -- turn command
    d -> [a] -> c, -- case command
    String -> c, -- function call

    -- direction
    d, -- left dir
    d, -- right dir
    d, -- front dir

    -- alt
    pat -> [c] -> a, --

    -- patterns
    pat, -- empty pattern
    pat, -- lambda pattern
    pat, -- debris pattern
    pat, -- asteroid pattern
    pat, -- boundary pattern
    pat -- underscore pattern
  )

fold :: Algebra p r c d a pat -> Program -> p
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
    algebra :: Algebra Bool b c d e etc
    algebra =
      ( undefined,
        undefined,
        undefined,
        undefined,
        undefined,
        undefined,
        undefined,
        undefined,
        undefined,
        undefined,
        undefined,
        undefined,
        undefined,
        undefined,
        undefined,
        undefined,
        undefined,
        undefined,
        undefined
      )
