module Algebra where

import Data.List (nub, sort)
import Model

-- Exercise 5

type Algebra p r i c d a pat =
  ( -- program
    [r] -> p, --

    -- rule
    i -> [c] -> r, --

    -- commands
    c, -- go
    c, -- take command
    c, -- mark command
    c, -- nothing command
    d -> c, -- turn command
    d -> [a] -> c, -- case command
    Identifier -> c, -- function call

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
    pat, -- underscore pattern

    -- identifier
    Identifier -> i --
  )

fold :: Algebra p r i c d a pat -> Program -> p
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
    underscore,
    identifier
    ) = f
    where
      f (Program rules) = prog (map fr rules)
      fr (Rule ident rules) = rule (fi ident) (map fc rules)
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
      fi ident = identifier ident

-- Exercise 6

checkProgram :: Program -> Bool
checkProgram = fold algebra
  where
    --         Algebra p    r                    i      c        d    a               pat
    algebra :: Algebra Bool (String, [[String]]) String [String] () (Pattern, [String]) Pattern
    algebra =
      ( program,
        (,), -- rule
        [], -- go
        [], -- take
        [], -- mark
        [], -- nothing
        const [], -- turn
        case',
        \(Identifier str) -> [str], 
        (), -- or undefined, doesn't matter
        (),
        (),
        \pat cmds -> (pat, concat cmds),
        EmptyPattern,
        LambdaPattern,
        DebrisPattern,
        AsteroidPattern,
        BoundaryPattern,
        UnderscorePattern,
        \(Identifier str) -> str
      )
      where
        -- Check whether a program is valid
        program :: [(String, [[String]])] -> Bool
        program xs =
          "start" `elem` ruleNames -- There is at least one start function
            && ruleNames == nub ruleNames -- No rule is defined twice
            && all (`elem` ruleNames) funcCalls -- There are no calls to undefined functions
          where
            ruleNames = map fst xs
            funcCalls = concat $ concatMap snd xs

        case' :: () -> [(Pattern, [String])] -> [String]
        case' _ pats
          | checkPatterns = concatMap snd pats
          | otherwise = ["!error"] -- kinda shitty but it works
          where
            cases = map fst pats
            checkPatterns
              -- Always fails if there are dupes
              | cases /= nub cases = False
              -- Always valid if there is an underscore pattern
              | UnderscorePattern `elem` cases = True
              | otherwise = length cases == 5
