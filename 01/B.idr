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
findComplementingProduct [] = 0
findComplementingProduct (x :: xs) = 
  let y = findComplement (2020 - x) xs in 
      if y > 0 then x * y * (2020 - x - y) else findComplementingProduct xs

main : IO ()
main = do file <- readFile "input.txt"
          case file of
               Right content => printLn (findComplementingProduct (convertToInts content))
               Left err => printLn err
