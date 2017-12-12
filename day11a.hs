import Data.String

-- Inputs are the match
count :: String -> Int -> [String] -> Int
count _ n [] = n
count m n (s:ss) = case (s==m) of True -> count m (n+1) ss
                                  False -> count m n ss

main = do
  fdat <- readFile "day11.txt"
  let ln = head ( lines fdat)
  let lns = words (map (\x -> case x of ',' -> ' '; v -> v) ln)
  --
  -- Assumes net n/ne/se values are positive, true for our input.
  let cts = map (\x -> count x 0 lns) ["n","ne","se","s","sw","nw"]
  let nval = (cts !! 0)-(cts !! 3)
  let neval = (cts !! 1)-(cts !! 4)
  let seval = (cts !! 2)-(cts !! 5)
  --
  let res = (nval+neval) -- Because seval is smallest.
  putStrLn (show res)
