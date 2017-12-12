import Data.String
import Data.List

-- Inputs are the match
count :: String -> Int -> [String] -> Int
count _ n [] = n
count m n (s:ss) = case (s==m) of True -> count m (n+1) ss
                                  False -> count m n ss

-- Convert hex coordinates to distance.
dmap :: Int -> Int -> Int -> Int
dmap n 0 0 = abs n
dmap 0 n 0 = abs n
dmap 0 0 n = abs n
dmap n ne se = do
  let newn = n-se
  let newne = ne+se
  case (newn >= 0) of True -> case (newne >= 0) of True -> (newn+newne)
                                                   False -> max newn (abs newne)
                      False -> case (newne >= 0) of True -> max (-newn) newne
                                                    False -> (-(newn+newne))

netlen :: [String] -> Int
netlen s = do
  let cts = map (\x -> count x 0 s) ["n","ne","se","s","sw","nw"]
  let nval = (cts !! 0)-(cts !! 3)
  let neval = (cts !! 1)-(cts !! 4)
  let seval = (cts !! 2)-(cts !! 5)
  dmap nval neval seval
  
main = do
  fdat <- readFile "day11.txt"
  let ln = head ( lines fdat)
  let lns = words (map (\x -> case x of ',' -> ' '; v -> v) ln)
  --
  -- Insane n-squared algorithm.  Really should accumulate distances
  -- during construction.
  let dstns = map netlen (inits lns)
  putStrLn (show (maximum dstns))
