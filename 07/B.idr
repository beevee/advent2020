module Main

data BagCount = Count Integer String

Show BagCount where
  show (Count n s) = show n ++ " " ++ show s

data BagRule = Rule String (List BagCount)

Show BagRule where
  show (Rule x ys) = show x ++ " can contain " ++ show ys

parseRuleEnd : List String -> List BagCount
parseRuleEnd (n :: a :: b :: _ :: zs) = (Count (cast n) (a ++ " " ++ b)) :: parseRuleEnd zs
parseRuleEnd _ = []

parseRule : String -> BagRule
parseRule x = 
  case words x of
    (a :: b :: _ :: _ :: zs) => Rule (a ++ " " ++ b) (parseRuleEnd zs)
    _ => Rule "" []

getChildCounts : String -> List BagRule -> List BagCount
getChildCounts _ [] = []
getChildCounts y ((Rule z xs) :: rs) = 
  if y == z then xs ++ getChildCounts y rs else getChildCounts y rs

folder : Integer -> BagCount -> BagCount -> BagCount
folder c (Count x1 y1) (Count x2 _) = Count (x1 + c*x2) y1

foldChildBags : List BagRule -> BagCount -> BagCount
foldChildBags rs (Count x y) = foldl (folder x) (Count x y) $ map (foldChildBags rs) (getChildCounts y rs)

count : BagCount -> Integer
count (Count x _) = x

main : IO ()
main = do file <- readFile "input.txt"
          case file of
              Right content => printLn (count (foldChildBags (map parseRule $ lines content) (Count 1 "shiny gold")) - 1)
              Left err => printLn err
