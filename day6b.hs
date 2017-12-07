import Data.List
import qualified Data.Map as Map

-- Helper fn, find the first index at which value v occurs, 
-- starting from x.
firstdx :: Int -> Int -> Map.Map Int Int -> Int
firstdx x v m = do
  let iv = case (Map.lookup x m) of Just vv -> vv
                                    _ -> 0
  case (iv == v) of True -> x
                    False -> firstdx (x+1) v m 


iinc :: Int -> Maybe Int
iinc x = Just (x+1)

-- Helper, increment the map value at index x.
increm :: Int -> Map.Map Int Int -> Map.Map Int Int
increm x m = (Map.update iinc x m)

-- Distribute content v over the indices, starting from x, wrapping around.
distrib :: Int -> Int -> Map.Map Int Int -> Map.Map Int Int
distrib x 0 m = m
distrib x v m = do
  let m2 = increm x m
  let x2 = mod (x+1) (length m2)
  distrib x2 (v-1) m2

-- Do the state transition -- find the maximum value and 
-- distribute it amongst the others.
istep :: Map.Map Int Int -> Map.Map Int Int
istep m = do
  let mv = maximum (Map.elems m)
  let dx = firstdx 0 mv m
  let m2 = Map.insert dx 0 m
  distrib (mod (dx+1) (length m2)) mv m2

-- Recursively iterate on the maps and accumulate states until you
-- see a duplicate, then return the size of the duplicate loop.
checkify  :: Int -> Map.Map Int Int -> [Map.Map Int Int] -> Int
checkify c m l = do
  let nm = istep m
  case (elemIndex nm l) of Just v -> (c - v)
                           _ -> checkify (c+1) nm (l ++ [nm]) 

main = do
  fdat <- readFile "day6.txt"
  let lns = head (lines fdat)
  let lst = []
  let mp = Map.fromList ( zip (iterate (\x -> x+1) 0)    
                              (map (\x -> read x :: Int) (words lns)) )
  putStrLn (show (checkify 0 mp []))
