import Data.Char

zcheck :: (Int, Int) -> Int
zcheck (a,b) = if a==b then a else 0

rotate :: [Int] -> Int -> [Int]
rotate a 0 = a
rotate a n = rotate ( tail a ++ [head a] ) (n-1)

main = do 
  l <- readFile "day1.txt"
  let cs = map digitToInt (filter (\x -> isDigit x) l)
  let n = div (length cs) 2 
  putStrLn (show n)
  let ds = rotate cs n
  let prs = zip cs ds
  putStrLn (show (sum (map zcheck prs)))
