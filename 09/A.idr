module Main

import Data.String

doCast : Maybe Integer -> Integer
doCast Nothing = 0
doCast (Just val) = val

hasSum : Integer -> List Integer -> Bool
hasSum a [] = False
hasSum a (x :: xs) = if (a - x) `elem` xs then True else hasSum a xs

findInvalidNum : List Integer -> List Integer -> Integer
findInvalidNum [] _ = 0
findInvalidNum _ [] = 0
findInvalidNum bootstrap@(b :: bs) (num :: nums) = 
  if hasSum num bootstrap 
    then findInvalidNum (bs ++ [num]) nums 
    else num

main : IO ()
main = do file <- readFile "input.txt"
          case file of
              Right content => 
                let nums = map (doCast . parseInteger) $ lines content in
                  printLn $ findInvalidNum (take 25 nums) (drop 25 nums)
              Left err => printLn err
