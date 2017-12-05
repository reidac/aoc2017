import Data.Char
import Data.String
import Data.List


anacheck :: (String, String) -> Bool
anacheck a = do
  let pmts = permutations (snd a)
  case elemIndex (fst a) pmts of Just n -> True
                                 _ -> False
        

dupcount :: [String] -> Int
dupcount a = do
  let prs = [ (x,y) | (x:ys) <- tails a, y <- ys ]
  sum ( map (\z -> if anacheck z then 1 else 0 ) prs )

vcount :: String -> Int
vcount a = if (dupcount (words a)) == 0 then 1 else 0
  

main = do
  fct <- readFile "day4.txt"
  let lns = lines fct
  let ckdat = map vcount lns
  putStrLn (show (sum (ckdat)))
