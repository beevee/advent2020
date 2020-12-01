module Main

import Data.String

doCast : Maybe Integer -> Integer
doCast Nothing = 0
doCast (Just val) = val

convertToInts : String -> List Integer
convertToInts s = map doCast (map parseInteger (words s))

findComplement : Integer -> List Integer -> Integer
findComplement _ [] = 0
findComplement x (y :: ys) = if elem (x - y) ys then y else (findComplement x ys)

findComplementingProduct : List Integer -> Integer
findComplementingProduct x = let c = findComplement 2020 x in c * (2020 - c)

main : IO ()
main = do file <- readFile "input.txt"
          case file of
               Right content => printLn (findComplementingProduct (convertToInts content))
               Left err => printLn err
