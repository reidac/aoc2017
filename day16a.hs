import Data.List


spin :: String -> String -> String
spin m s = do
  let ln = (length s) - (read (tail m) :: Int)
  (drop ln s) ++ (take ln s)

xchange :: String -> String -> String
xchange m s = do
  let slts = map (\x -> read x :: Int ) (words ( map (\x -> if x == '/' then ' ' else x) (tail m)))
  let (p1,p2) = ((minimum slts),(maximum slts))
  let perms = [0..(p1-1)]++[p2]++[(p1+1)..(p2-1)]++[p1]++[(p2+1)..((length s)-1)]
  map (\x -> (!!) s x) perms
  

partnr :: String -> String -> String
partnr m s = do
  let slnms = (words ( map (\x -> if x == '/' then ' ' else x) (tail m)))
  let slts = [findIndex (\x -> x == head (slnms !! 0)) s,
              findIndex (\x -> x == head (slnms !! 1)) s]
  let rslts = map (\x -> case x of Just v -> v; _ -> -1) slts
  let (p1,p2) = ((minimum rslts),(maximum rslts))
  let perms = [0..(p1-1)]++[p2]++[(p1+1)..(p2-1)]++[p1]++[(p2+1)..((length s)-1)]
  map (\x -> (!!) s x) perms
         

shffle :: [String] -> String -> String
shffle [] r = r
shffle (m:ms) r = case (head m) of
  's' -> shffle ms (spin m r)
  'x' -> shffle ms (xchange m r)
  'p' -> shffle ms (partnr m r)


main = do
  fdat <- readFile "day16.txt"
  let mvs = words (map (\x -> if x == ',' then ' ' else x) (head (lines fdat)))
  let res = shffle mvs "abcdefghijklmnop"
  putStrLn (show res)
