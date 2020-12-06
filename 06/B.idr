module Main

import Data.List

splitFileIntoGroups : String -> List (List String)
splitFileIntoGroups x = splitOn "" (lines x)

countEveryonesChars : List String -> Nat
countEveryonesChars x = length $ foldl1 intersect $ map unpack x

main : IO ()
main = do file <- readFile "input.txt"
          case file of
              Right content => printLn $ sum $ map countEveryonesChars $ splitFileIntoGroups content
              Left err => printLn err
