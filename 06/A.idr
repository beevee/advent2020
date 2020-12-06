module Main

splitFileIntoGroups : String -> List String
splitFileIntoGroups x = map concat (splitOn "" (lines x))

countUniqueChars : String -> Nat
countUniqueChars x = length $ nub $ unpack x

main : IO ()
main = do file <- readFile "input.txt"
          case file of
              Right content => printLn $ sum $ map countUniqueChars $ splitFileIntoGroups content
              Left err => printLn err
