module Main

import Data.String

data Entry = PasswordEntry Integer Integer Char String

doCast : Maybe Integer -> Integer
doCast Nothing = 0
doCast (Just val) = val

intLength : List _ -> Integer
intLength l = toIntegerNat $ length l

convertToEntry : String -> Entry
convertToEntry s = 
  let parsedFields = split (not . isAlphaNum) s
      (minLen :: maxLen :: ch :: _ :: pass :: _) = parsedFields in
  PasswordEntry (doCast $ parseInteger minLen) (doCast $ parseInteger maxLen) (strHead ch) pass

convertToEntries : String -> List Entry
convertToEntries s = map convertToEntry (lines s)

countChars : Char -> String -> Integer
countChars c s = intLength $ filter (== c) (unpack s)

isValid : Entry -> Bool
isValid (PasswordEntry minLen maxLen ch pass) = 
  let actLen = countChars ch pass in
      (actLen >= minLen) && (actLen <= maxLen)

countValidPasswords : List Entry -> Integer
countValidPasswords ps = intLength (filter isValid ps)

main : IO ()
main = do file <- readFile "input.txt"
          case file of
               Right content => printLn (countValidPasswords (convertToEntries content))
               Left err => printLn err
