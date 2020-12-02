module Main

import Data.String

data Entry = PasswordEntry Integer Integer Char String

doCast : Maybe Integer -> Integer
doCast Nothing = 0
doCast (Just val) = val

doCastChar : Maybe Char -> Char
doCastChar Nothing = '-'
doCastChar (Just val) = val

intLength : List _ -> Integer
intLength l = toIntegerNat $ length l

convertToEntry : String -> Entry
convertToEntry s = 
  let parsedFields = split (not . isAlphaNum) s
      (fstIdx :: sndIdx :: ch :: _ :: pass :: _) = parsedFields in
  PasswordEntry (doCast $ parseInteger fstIdx) (doCast $ parseInteger sndIdx) (strHead ch) pass

convertToEntries : String -> List Entry
convertToEntries s = map convertToEntry (lines s)

charAtIndex : Integer -> String -> Char
charAtIndex i s = doCastChar $ index' (fromIntegerNat (i-1)) (unpack s)

isValid : Entry -> Bool
isValid (PasswordEntry fstIdx sndIdx ch pass) = 
  let fstCh = charAtIndex fstIdx pass
      sndCh = charAtIndex sndIdx pass in
      (fstCh == ch && sndCh /= ch) || (fstCh /= ch && sndCh == ch)

countValidPasswords : List Entry -> Integer
countValidPasswords ps = intLength (filter isValid ps)

main : IO ()
main = do file <- readFile "input.txt"
          case file of
               Right content => printLn (countValidPasswords (convertToEntries content))
               Left err => printLn err
