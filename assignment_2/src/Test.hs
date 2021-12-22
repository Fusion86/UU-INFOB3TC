module Test where

import Algebra
import Common
import Data.List (isSuffixOf)
import Data.Map ((!))
import Interpreter
import Lexer
import ParseLib.Abstract (parse)
import Parser
import System.Directory (getDirectoryContents)

test :: IO ()
test = do
  examples <- getDirectoryContents "./examples"
  let fullPaths = map ("./examples/" ++) examples
      progs = filter (".arrow" `isSuffixOf`) fullPaths
  mapM_ testCheckProgram progs
  where
    testCheckProgram :: String -> IO ()
    testCheckProgram f = do
      str <- readFile f
      let res = checkProgram (parseProgram (alexScanTokens str))
          resStr = if res then "succeeded" else "failed"
      print $ "checkProgram: \"" ++ f ++ "\" " ++ resStr
      return ()

testSourceWithSpace :: String -> String -> Pos -> Heading -> IO ()
testSourceWithSpace a b pos heading = do
  sourceStr <- readFile a
  spaceStr <- readFile b

  let env = toEnvironment sourceStr
      space = fst (head (parse parseSpace spaceStr))
      startCmds = env ! "start"
      state = ArrowState space pos heading startCmds

  print "Environment:"
  print env
  print "" -- newline
  let res = run env state
      space = getSpace res

  pPrint res

  case getSpace res of
    Nothing -> print "Can't print space because the program failed."
    Just x -> do
      print "Space:"
      print $ printSpace x
  return ()
  where
    getSpace (Done space _ _) = Just space
    getSpace _ = Nothing

    run :: Environment -> ArrowState -> Step
    run env state = f $ step env state
      where
        f res@Done {} = res
        f res@Fail {} = res
        f (Ok newState) = run env newState

testDebris :: IO ()
testDebris = do
  testSourceWithSpace "./examples/RemoveDebris.arrow" "./examples/SampleSpace.space" (2, 3) South

testAdd :: IO ()
testAdd = do
  testSourceWithSpace "./examples/Add.arrow" "./examples/AddInput.space" (0, 0) East

testParser :: IO ()
testParser = do
  str <- readFile "./examples/RemoveDebris.arrow"
  pPrint $ parseProgram (alexScanTokens str)
  return ()
