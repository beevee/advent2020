module Main

findJoltDiffs : (Integer, Integer) -> List Integer -> (Integer, Integer)
findJoltDiffs (one, three) (x :: y :: zs) =
  let (one', three') = findJoltDiffs (one, three) (y :: zs) in
    if (y - x) == 1 then (one' + 1, three') else (one', three' + 1)
findJoltDiffs (one, three) _ = (one, three + 1)

multJoltDiffs : List Integer -> Integer
multJoltDiffs jolts =
  let (one, three) = findJoltDiffs (0, 0) (0 :: jolts) in
    one * three

main : IO ()
main = do file <- readFile "input.txt"
          case file of
              Right content => 
                  printLn $ multJoltDiffs $ sort $ map ((the Integer) . cast) $ lines content
              Left err => printLn err
