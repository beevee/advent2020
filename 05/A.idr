module Main

convertPassToInteger' : List Char -> Integer -> Integer
convertPassToInteger' [] k = 0
convertPassToInteger' (x :: xs) k = 
  let rest = convertPassToInteger' xs (k `div` 2) in
    case x of
      'F' => rest
      'B' => k + rest
      'R' => k + rest
      'L' => rest

convertPassToInteger : String -> Integer
convertPassToInteger s = convertPassToInteger' (unpack s) 512

main : IO ()
main = do file <- readFile "input.txt"
          case file of
              Right content => printLn $ foldl1 max $ map convertPassToInteger (lines content)
              Left err => printLn err
