
-- Really want to make an instance of the Num typeclass so I
-- can do arithmetic with coords, but can't get there.

import qualified Data.Map as Map
import Data.Char

gridcol :: Int -> Int -> String -> Map.Map (Int, Int) Char
        -> Map.Map (Int, Int) Char
gridcol _ _ "" m = m
gridcol r c (s:ss) m = gridcol r (c+1) ss (Map.insert (r,c) s m)

gridrow :: Int -> [String] -> Map.Map (Int, Int) Char -> Map.Map (Int, Int) Char
gridrow _ [] m = m
gridrow r (s:ss) m = gridrow (r+1) ss (gridcol r 0 s m)

startpt :: (Int, Int) -> Map.Map (Int, Int) Char -> (Int, Int)
startpt (r,c) m = if (Map.findWithDefault ' ' (r,c) m) == '|' then (r,c)
  else startpt (r,(c+1)) m

-- Turning commands...
left :: (Int, Int) -> (Int, Int)
left (1,0) = (0,1)
left (0,1) = (-1,0)
left (-1,0) = (0,-1)
left (0,-1) = (1,0)

right :: (Int, Int) -> (Int, Int)
right (1,0) = (0,-1)
right (0,-1) = (-1,0)
right (-1,0) = (0,1)
right (0,1) = (1,0)

trverse :: (Int, Int) -> (Int, Int) -> Map.Map (Int, Int) Char -> String -> String
trverse (cr,cc) (fr,fc) m s = do
  let cchar = Map.findWithDefault ' ' (cr,cc) m
  if (cchar == ' ') then s else do -- Terminate when you run off the end.
    let fchar = Map.findWithDefault ' ' (cr+fr,cc+fc) m
    if (cchar /= '+') then trverse (cr+fr, cc+fc) (fr,fc) m (s++[cchar]) else do
      let (lr,lc) = left (fr,fc)
      let (rr,rc) = right (fr,fc)
      let lchar = Map.findWithDefault ' ' (cr+lr,cc+lc) m
      -- let rchar = Map.findWithDefault ' ' (cr+rr,cc+rc) m
      if (lchar /= ' ') then trverse (cr+lr, cc+lc) (lr, lc) m (s++[cchar])
                        else trverse (cr+rr, cc+rc) (rr, rc) m (s++[cchar])
      

main = do
  fdat <- readFile "day19.txt"
  let m = gridrow 0 (lines fdat) Map.empty
  let (sr,sc) = startpt (0,0) m
  let res = trverse (sr,sc) (1,0) m ""
  putStrLn (show (length res))

                                 
