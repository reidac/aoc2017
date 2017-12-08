import Data.List
import Data.String
import qualified Data.Map as Map

-- Custom composite data type for the target of the map.
-- I suppose one could build an actual tree, but that seems complicated.
-- Besides, I already know how to do maps.
data Cnode = Cnode Int [String] deriving (Show)

nodewgt :: Cnode -> Int
nodewgt (Cnode n _) = n

nodelst :: Cnode -> [String]
nodelst (Cnode _ l) = l


parsify :: String -> Map.Map String Cnode -> Map.Map String Cnode
parsify s m = do
  let wds = words s
  let lbl = head wds
  let wgt = read (drop 1 (take ((length (wds !! 1)) -1 ) (wds !! 1))) :: Int
  let tlist = if ((length wds) > 3) then (drop 3 wds) else []
  Map.insert lbl (Cnode wgt tlist) m

buildify :: [String] -> Map.Map String Cnode -> Map.Map String Cnode
buildify [] m = m
buildify l m = buildify (tail l) (parsify (head l) m)

treewgt :: String -> Map.Map String Cnode -> Int
treewgt l m = do
  let labelnode = case (Map.lookup l m) of Just v -> v
                                           _ -> (Cnode 0 [])
  let labelwgt = nodewgt labelnode
  if (length (nodelst labelnode)) == 0
    then labelwgt
    else sum (map (\x -> treewgt x m) (nodelst labelnode)) + labelwgt

subtreeswgt :: String -> Map.Map String Cnode -> [(String, Int)]
subtreeswgt l m = do
  let labelnode = case (Map.lookup l m) of Just v -> v
                                           _ -> (Cnode 0 [])
  zip (nodelst labelnode) (map (\x -> treewgt x m) (nodelst labelnode))

-- Annoying solution, does not generalize to other inputs --
-- these values were obtained by "manually" traversing the
-- tree.
main = do 
  fdat <- readFile "day7c.txt" -- Version with commas removed.
  let lns = lines fdat
  let mp = buildify lns Map.empty
  putStrLn (show (treewgt "gynfwly" mp))
  putStrLn (show (subtreeswgt "gynfwly" mp))
  putStrLn (show (subtreeswgt "jjjks" mp))
  putStrLn (show (subtreeswgt "gtervu" mp))
  putStrLn (show (subtreeswgt "ycbgx" mp))
  let tgtnode = case (Map.lookup "ycbgx" mp) of Just v -> v
                                                _ -> (Cnode 0 [])
  putStrLn (show (nodewgt tgtnode))
