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

main = do
  fdat <- readFile "day13.txt"
  let mp = mappify Map.empty (lines fdat)
  --
  putStrLn (show mp)
  --
  let res = Map.foldrWithKey (\k v r -> if ((spos v k) == 0) then r+k*v else r) 0 mp
  putStrLn (show res)
