import Data.String

lrotate :: Int -> [Int] -> [Int]
lrotate v l = (drop v l) ++ (take v l)

lprocess :: Int -> [Int] -> [Int] -> [Int]
lprocess sk [] d = d
lprocess sk lg d = do
  let lgn = (head lg)
  let rt = mod (lgn + sk) (length d)
  let nl = ((reverse (take lgn d)) ++ (drop lgn d))
  let pnl = (lrotate rt nl)
  let qnl = (lprocess (sk+1) (tail lg) pnl)
  lrotate ((length qnl) - rt) qnl 
  

main = do
  fdat <- readFile "day10c.txt" -- Comma-removed version.
  let lengths = map (\x -> read x :: Int) (words (head (lines fdat)))
  --
  let datalist = [0..255]
  --
  let prodlist = lprocess 0 lengths datalist
  --
  putStrLn (show prodlist)
  putStrLn (show (( prodlist !! 0) * (prodlist !! 1)))
