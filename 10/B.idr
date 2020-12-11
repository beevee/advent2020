module Main

countJoltPerms : List Integer -> Integer
countJoltPerms (a :: b :: c :: d :: e :: rest) = 
  if (e - a) < 5
    then 7 * countJoltPerms rest
    else if (d - a) < 4
           then 4 * countJoltPerms (e :: rest)
           else if (c - a) < 3
                  then 2 * countJoltPerms (d :: e :: rest)
                  else countJoltPerms (b :: c :: d :: e :: rest)
countJoltPerms (a :: b :: c :: d :: rest) =
  if (d - a) < 4
    then 4 * countJoltPerms rest
    else if (c - a) < 3
           then 2 * countJoltPerms (d :: rest)
           else countJoltPerms (b :: c :: d :: rest)
countJoltPerms (a :: b :: c :: rest) =
  if (c - a) < 3
    then 2 * countJoltPerms rest
    else countJoltPerms (b :: c :: rest)
countJoltPerms _ = 1

main : IO ()
main = do file <- readFile "input.txt"
          case file of
              Right content => 
                  printLn $ countJoltPerms (0 :: (sort $ map ((the Integer) . cast) $ lines content))
              Left err => printLn err
