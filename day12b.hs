import Data.String
import qualified Data.Map as Map
import qualified Data.Set as Set


-- For each string in the list of strings, create the
-- appropriate map entry in the map, index is the node,
-- value is the list of nodes that can be directly reached.
mappify :: Map.Map Int [Int] -> [String] -> Map.Map Int [Int]
mappify m [] = m
mappify m (s:ss) = do
  let wds = words (map (\x -> case x of ',' -> ' '; v -> v) s)
  let key = read (head wds) :: Int
  let vlist = map (\x -> read x :: Int) (drop 2 wds)
  let nm = Map.insert key vlist m
  mappify nm ss

-- Given a map and an index, return the set of all node numbers
-- that can be reached from here.
trverse :: Map.Map Int [Int] -> Int -> Set.Set Int -> Set.Set Int
trverse m k s = do
  if Set.member k s then s else do
    let s2 = Set.insert k s
    let chdrn = Map.findWithDefault [0] k m
    foldr Set.union s2 (map (\x -> trverse m x s2) chdrn)

-- Remove an arbitrary group from the map, keeping going until the
-- map is empty, count up how many groups you removed.
delgrp :: Map.Map Int [Int] -> Int -> Int
delgrp m n = if (Map.null m) then n else do
  let k = head (Map.keys m) -- Just grab the first one.
  let s = trverse m k Set.empty
  let nm = foldr Map.delete m (Set.toList s) -- Delete the traversed set.
  delgrp nm (n+1) 

main = do
  fdat <- readFile "day12.txt"
  let mp = mappify Map.empty (lines fdat)
  --
  putStrLn (show (delgrp mp 0))
