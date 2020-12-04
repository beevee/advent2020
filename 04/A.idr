module Main

splitFileIntoPassports : String -> List String
splitFileIntoPassports c = map unwords (splitOn "" (lines c))

countRequiredFields : List (List String) -> Nat
countRequiredFields [] = 0
countRequiredFields (f :: fs) = 
  let other = countRequiredFields fs in
      case f of
        [] => other
        (k :: _) => other + if k == "byr" || 
                               k == "iyr" ||
                               k == "eyr" ||
                               k == "hgt" ||
                               k == "hcl" ||
                               k == "ecl" ||
                               k == "pid" then 1 else 0

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
