module Assignment0 where

import Test.Read (readMaybe)

-- 1 
unwordsDC :: [String] -> String
unwordsDC [] = ""
unwordsDC (x:xs) = x ++ " " ++ unwordsDC xs

unwordsFoldr :: [String] -> String
unwordsFoldr xs = foldr (\a b -> a ++ " " ++ b) "" xs

wordsH :: String -> [String]
wordsH "" = []
wordsH str =
  let part = takeWhile (/= ' ') str
      next = drop (length part + 1) str
      spaceCount = length (takeWhile (== ' ') next)
  in part : wordsH (drop spaceCount next)

wordsFoldr :: String -> [String]
wordsFoldr str = foldr f [] str
  where
    f a b = undefined

-- 2
-- Nah

-- 3
f :: [Int] -> Int
f xs = snd $ foldr (\b (t,v) -> (t * 10, v + (b * t))) (1,0) xs

f2 :: String -> Int
f2 str = f (map (\s -> read s :: Int) (wordsH str))

-- 4

data Tree a = 
  Bin (Tree a) (Tree a)
  | Tip a

information :: Tree a -> [a]
information (Tip a) = [a]
information (Bin l r) = information l ++ information r

-- Test using:
-- pack (Tip 3)
-- pack (Bin (Bin (Tip 3) (Bin (Tip 4) (Tip 5))) (Bin (Tip 4) (Tip 5)))
pack :: Tree Int -> String
pack (Tip a) = show a
pack (Bin l r) = "{" ++ pack l ++ "," ++ pack r ++ "}"

-- Test using:
-- unpack (pack testSetFromAbove)
unpack :: String -> Tree Int
unpack str = undefined
