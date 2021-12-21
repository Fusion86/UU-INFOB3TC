module Test where

import Algebra
import Data.List (isSuffixOf)
import Lexer
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
