module Main

import Data.String

splitFileIntoPassports : String -> List String
splitFileIntoPassports c = map unwords (splitOn "" (lines c))

byrIsValid : String -> Bool
byrIsValid s = case parseInteger s of
  Nothing => False
  Just val => val >= 1920 && val <= 2002

iyrIsValid : String -> Bool
iyrIsValid s = case parseInteger s of
  Nothing => False
  Just val => val >= 2010 && val <= 2020

eyrIsValid : String -> Bool
eyrIsValid s = case parseInteger s of
  Nothing => False
  Just val => val >= 2020 && val <= 2030

hgtIsValid : String -> Bool
hgtIsValid s = 
  let units = pack $ filter isAlpha (unpack s)
      amount = pack $ filter isDigit (unpack s) in
      case parseInteger amount of
           Nothing => False
           Just val => case units of
                            "cm" => val >= 150 && val <= 193
                            "in" => val >= 59 && val <= 76
                            _ => False

isHexNum : List Char -> Bool
isHexNum [] = True
isHexNum (x :: xs) = if isDigit x || (x >= 'a' && x <= 'f') then isHexNum xs else False

hclIsValid : String -> Bool
hclIsValid s = 
  let unpacked = unpack s in
      case unpacked of
        [] => False
        (x :: xs) => (x == '#') && (length xs == 6) && (isHexNum xs)

eclIsValid : String -> Bool
eclIsValid s = s == "amb" || 
               s == "blu" || 
               s == "brn" || 
               s == "gry" || 
               s == "grn" || 
               s == "hzl" || 
               s == "oth"

pidIsValid : String -> Bool
pidIsValid s = length s == 9 &&
  case parseInteger s of
    Nothing => False
    Just val => True

countRequiredFields : List (List String) -> Nat
countRequiredFields [] = 0
countRequiredFields (f :: fs) = 
  let other = countRequiredFields fs in
      case f of
        [] => other
        (k :: []) => other
        (k :: v :: _) => other + if (k == "byr" && byrIsValid v) || 
                                    (k == "iyr" && iyrIsValid v) ||
                                    (k == "eyr" && eyrIsValid v) ||
                                    (k == "hgt" && hgtIsValid v) ||
                                    (k == "hcl" && hclIsValid v) ||
                                    (k == "ecl" && eclIsValid v) ||
                                    (k == "pid" && pidIsValid v) then 1 else 0

countValidPassport : String -> Nat
countValidPassport p = 
  let fields = map (split (== ':')) (words p) in
      if countRequiredFields fields == 7 then 1 else 0

countValidPassports : List String -> Nat
countValidPassports ps = sum $ map countValidPassport ps

main : IO ()
main = do file <- readFile "input.txt"
          case file of
               Right content => printLn $ countValidPassports $ splitFileIntoPassports content
               Left err => printLn err
