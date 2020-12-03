module Main

import Data.String

nextPosition : Nat -> Nat -> Nat
nextPosition l a = let newA = a + 3 in
  if newA >= l then newA `minus` l else newA

countTreesInLine : String -> Nat -> Nat
countTreesInLine s k = case index' k (unpack s) of
                            Just ch => if ch == '#' then 1 else 0
                            Nothing => 0

countTrees : List String -> Nat -> Nat
countTrees [] _ = 0
countTrees (s :: ss) a = (countTreesInLine s a) + (countTrees ss (nextPosition (length s) a))

main : IO ()
main = do file <- readFile "input.txt"
          case file of
               Right content => printLn (countTrees (lines content) 0)
               Left err => printLn err
