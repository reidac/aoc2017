import Data.Char

zcheck :: (Int, Int) -> Int
zcheck (a,b) = if a==b then a else 0

main = do 
  l <- readFile "day1.txt"
  let cs = filter (\x -> isDigit x) l
  let ds = tail cs ++ [head cs]
  let prs = zip (map digitToInt cs) (map digitToInt ds)
  putStrLn (show (sum (map zcheck prs)))
