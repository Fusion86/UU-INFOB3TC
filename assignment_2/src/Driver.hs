module Driver where

import Algebra
import Interpreter
import Model
import Data.Char

-- Exercise 11
interactive :: Environment -> ArrowState -> IO ()
interactive env state = do
  print "Current state:"
  printArrowState state
  putStr "Continue execution? [Y/n]: "
  x <- getLine

  if map toLower x `elem` ["y", ""] then do
    let res = step env state
    
    case res of
      (Done space pos heading) -> do
        print "Execution finished."
        printArrowState state
        print "Thanks for flying with us\
  \\
  \                     `. ___\
  \                    __,' __`.                _..----....____\
  \        __...--.'``;.   ,.   ;``--..__     .'    ,-._    _.-'\
  \  _..-''-------'   `'   `'   `'     O ``-''._   (,;') _,'\
  \,'________________                          \\`-._`-','\
  \ `._              ```````````------...___   '-.._'-:\
  \    ```--.._      ,.                     ````--...__\\-.\
  \            `.--. `-`                       ____    |  |`\
  \              `. `.                       ,'`````.  ;  ;`\
  \                `._`.        __________   `.      \\'__/`\
  \                   `-:._____/______/___/____`.     \\  `\
  \                               |       `._    `.    \\\
  \                               `._________`-.   `.   `.___\
  \                                             SSt  `------'`"
      (Ok state) -> do
        interactive env state
      (Fail err) -> do
        print "Execution failed!"
        print $ "Error: " ++ err
  else do -- this works???
  return ()

printArrowState :: ArrowState -> IO ()
printArrowState (ArrowState space pos heading stack) = do
  print $ "Position: " ++ show pos
  print $ "Heading: " ++ show heading
  print "Current space:"
  print $ printSpace space

batch :: Environment -> ArrowState -> (Space, Pos, Heading)
batch env state@(ArrowState space pos heading stack) = f $ step env state
  where
    f (Done space pos heading) = (space, pos, heading)
    f (Fail err)= error $ "Execution failed: " ++ err
    f (Ok newState) = batch env newState
