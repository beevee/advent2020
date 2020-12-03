module Main

import Data.String

nextPosition : Nat -> Nat -> Nat -> Nat
nextPosition l a b = let newA = a + b in
  if newA >= l then newA `minus` l else newA

countTreesInLine : String -> Nat -> Nat
countTreesInLine s k = case index' k (unpack s) of
                            Just ch => if ch == '#' then 1 else 0
                            Nothing => 0

countTrees : List String -> Nat -> Nat -> Nat
countTrees [] _ _ = 0
countTrees (s :: ss) a b = (countTreesInLine s a) + (countTrees ss (nextPosition (length s) a b) b)

oddElements : List elem -> List elem
oddElements (x :: y :: xs) = x :: oddElements xs
oddElements xs = xs

multiplyAllTrajectories : List String -> Nat
multiplyAllTrajectories ss = let firstTrjCount = countTrees ss 0 1
                                 secondTrjCount = countTrees ss 0 3 
                                 thirdTrjCount = countTrees ss 0 5 
                                 fourthTrjCount = countTrees ss 0 7 
                                 fifthTrjCount = countTrees (oddElements ss) 0 1 in
                                 firstTrjCount * secondTrjCount * thirdTrjCount * fourthTrjCount * fifthTrjCount

main : IO ()
main = do file <- readFile "input.txt"
          case file of
               Right content => printLn (multiplyAllTrajectories (lines content))
               Left err => printLn err
