module Main

import Data.String

doCast : Maybe Integer -> Integer
doCast Nothing = 0
doCast (Just val) = val

hasSum : Integer -> List Integer -> Bool
hasSum a [] = False
hasSum a (x :: xs) = if (a - x) `elem` xs then True else hasSum a xs

findSumSet' : Integer -> List Integer -> List Integer -> List Integer
findSumSet' n nums [] = 
  let curSum = sum nums in
    if curSum > n 
      then findSumSet' n (drop 1 nums) [] 
      else (if curSum < n then [] else nums)
findSumSet' n nums (r :: rest) = 
  let curSum = sum nums in
    if curSum > n 
      then findSumSet' n (drop 1 nums) (r :: rest) 
      else (if curSum < n then findSumSet' n (nums ++ [r]) rest else nums)

findSumSet : Integer -> List Integer -> List Integer
findSumSet n nums = findSumSet' n [] nums

main : IO ()
main = do file <- readFile "input.txt"
          case file of
              Right content => 
                let nums = map (doCast . parseInteger) $ lines content 
                    sumSet = findSumSet 69316178 nums in
                  printLn $ (foldl1 max sumSet) + (foldl1 min sumSet)
              Left err => printLn err

              -- 69316178
