module Main

data BagRule = Rule String (List String)

Show BagRule where
  show (Rule x ys) = show x ++ " can contain " ++ show ys

parseRuleEnd : List String -> List String
parseRuleEnd (_ :: a :: b :: _ :: zs) = (a ++ " " ++ b) :: parseRuleEnd zs
parseRuleEnd _ = []

parseRule : String -> BagRule
parseRule x = 
  case words x of
    (a :: b :: _ :: _ :: zs) => Rule (a ++ " " ++ b) (parseRuleEnd zs)
    _ => Rule "" []

findParents : String -> List BagRule -> List String
findParents _ [] = []
findParents s ((Rule x ys) :: rs) = 
  if s `elem` ys then x :: findParents s rs else findParents s rs

findAllPredecessors' : List String -> List BagRule -> List String
findAllPredecessors' [] _ = []
findAllPredecessors' (x :: xs) ys = findParents x ys ++ findAllPredecessors' xs ys

findAllPredecessors : List String -> List BagRule -> List String
findAllPredecessors [] _ = []
findAllPredecessors xs ys = 
  case findAllPredecessors' xs ys of
    [] => xs
    ps => nub $ xs ++ findAllPredecessors ps ys

main : IO ()
main = do file <- readFile "input.txt"
          case file of
              Right content => printLn $ minus (length $ findAllPredecessors ["shiny gold"] $ map parseRule $ lines content) 1
              Left err => printLn err
