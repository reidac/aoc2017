import Data.String
import qualified Data.Map as Map

-- Map is indexed by firewall layer, and value'd by
-- scanner length.  Not every layer has a scanner.
mappify :: Map.Map Int Int -> [String] -> Map.Map Int Int
mappify m [] = m
mappify m (l:ls) = do
  let [k,v] = map (\x -> read x :: Int) (words (map (\x -> case x of ':' -> ' '; v -> v) l))
  mappify (Map.insert k v m) ls

-- Position of a scanner at the start of increment t.
-- Inputs are the scanner length and the current increment.
spos :: Int -> Int -> Int
spos 0 _ = 0
spos l t = do
  let ph = mod t (2*(l-1))
  if (ph < l) then ph else ((2*(l-1))-ph)

-- Score function -- traverse the firewall with a delay of d, count
-- how many times you are caught.  NB this is *not* the severity,
-- in this case, getting caught at level zero still counts!
score :: Int -> Map.Map Int Int -> Int
score d m = Map.foldrWithKey (\k v r -> if ((spos v (k+d)) == 0) then r+1 else r) 0 m

firstzero :: Map.Map Int Int -> [Int] -> Int
firstzero m (v:vs) = if (score v m) == 0 then v else firstzero m vs

main = do
  fdat <- readFile "day13.txt"
  let mp = mappify Map.empty (lines fdat)
  --
  putStrLn (show mp)
  --
  let res= firstzero mp (iterate (\x -> x+1) 0) 
  --
  putStrLn (show res)
