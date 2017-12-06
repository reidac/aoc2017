import Data.List
import Data.Char
import Data.String
import qualified Data.Map as Map



-- Apparently the Maybe/Just is required by Map.update semantics.
iinc :: Int -> Maybe Int
iinc x = Just (x+1)

idec :: Int -> Maybe Int
idec x = Just (x-1)

-- index, step count, map, counts to halt.
haltstep :: Int -> Int -> Map.Map Int Int -> Int
haltstep x c m = do
  let oset = case (Map.lookup x m) of Just v -> v
                                      _ -> 0
  let nx = x + oset
  let m2 = case (oset >= 3) of True -> Map.update idec x m 
                               False -> Map.update iinc x m
  if (Map.member nx m2) then (haltstep nx (c+1) m2) else (c+1)

main = do 
  fdat <- readFile "day5.txt"
  let lns = lines fdat
  let m = Map.fromList ( zip (iterate (\x -> x+1) 0) 
                             (map (\x -> read x :: Int) lns) )
  putStrLn ( show ( haltstep 0 0 m ))
