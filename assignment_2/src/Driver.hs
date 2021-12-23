module Driver where

import Algebra
import Interpreter
import Model
import Data.Char
import Control.Monad (when)

-- Exercise 11
interactive :: Environment -> ArrowState -> IO ()
interactive env state = do
  putStrLn "Current state:"
  printArrowState state
  putStr "Continue execution? [Y/n]: "
  x <- getLine

  when (map toLower x `elem` ["y", ""]) $ do
    let res = step env state

    case res of
      (Done space pos heading) -> do
        putStrLn "Execution finished."
        printArrowState state
        putStrLn "Thanks for flying with us\
  \\n\
  \\n                     `. ___\
  \\n                    __,' __`.                _..----....____\
  \\n        __...--.'``;.   ,.   ;``--..__     .'    ,-._    _.-'\
  \\n  _..-''-------'   `'   `'   `'     O ``-''._   (,;') _,'\
  \\n,'________________                          \\`-._`-','\
  \\n `._              ```````````------...___   '-.._'-:\
  \\n    ```--.._      ,.                     ````--...__\\-.\
  \\n            `.--. `-`                       ____    |  |`\
  \\n              `. `.                       ,'`````.  ;  ;`\
  \\n                `._`.        __________   `.      \\'__/`\
  \\n                   `-:._____/______/___/____`.     \\  `\
  \\n                               |       `._    `.    \\\
  \\n                               `._________`-.   `.   `.___\
  \\n                                             SSt  `------'`"
      (Ok state) -> do
        interactive env state
      (Fail err) -> do
        putStrLn "Execution failed!"
        putStrLn $ "Error: " ++ err

printArrowState :: ArrowState -> IO ()
printArrowState (ArrowState space pos heading stack) = do
  putStrLn $ "Position: " ++ show pos
  putStrLn $ "Heading: " ++ show heading
  putStrLn "Current space:"
  putStrLn $ printSpace space

batch :: Environment -> ArrowState -> (Space, Pos, Heading)
batch env state@(ArrowState space pos heading stack) = f $ step env state
  where
    f (Done space pos heading) = (space, pos, heading)
    f (Fail err)= error $ "Execution failed: " ++ err
    f (Ok newState) = batch env newState
