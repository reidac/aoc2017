import Data.String
import qualified Data.Map as Map
import qualified Data.Set as Set


mappify :: Map.Map Int [Int] -> [String] -> Map.Map Int [Int]
mappify m [] = m
mappify m (s:ss) = do
  let wds = words (map (\x -> case x of ',' -> ' '; v -> v) s)
  let key = read (head wds) :: Int
  let vlist = map (\x -> read x :: Int) (drop 2 wds)
  let nm = Map.insert key vlist m
  mappify nm ss

trverse :: Map.Map Int [Int] -> Int -> Set.Set Int -> Set.Set Int
trverse m k s = do
  if Set.member k s then s else do
    let s2 = Set.insert k s
    let chdrn = Map.findWithDefault [0] k m
    foldr Set.union s2 (map (\x -> trverse m x s2) chdrn)
    

main = do
  fdat <- readFile "day12.txt"
  let mp = mappify Map.empty (lines fdat)
  --
  let s = trverse mp 0 Set.empty
  -- 
  putStrLn (show s)
  putStrLn (show (length s))
