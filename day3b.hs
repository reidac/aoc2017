
import qualified Data.Map as Map


-- Given an integer N, finds the spiral ring that it's in, and 
-- "how far around" it is, in that order, in the tuple.
ringfinder :: Int -> (Int, Int)
ringfinder 1 = (0,0)
ringfinder n = do
  let r = ringsearcher n 0
  (r, n-(enclosure (r-1))-1)


-- The "size" of a ring, i.e. how many numbers are inside the ring
-- of size x. 
enclosure :: Int -> Int
enclosure x = (2*x+1)*(2*x+1)

-- Helper function.  Given n and a candidate starting ring, returns the ring.
ringsearcher :: Int -> Int -> Int 
ringsearcher n k = if (ringp n k) then k else (ringsearcher n (k+1))

-- Ring predicate -- returns true if n is in ring at least k.  The
-- first k (from below) for which this predicte is true for a given
-- n is n's ring.
ringp :: Int -> Int -> Bool
ringp n k = if (div n (enclosure k+1) )==0 then True else False

-------------------

-- Work out the actual (x,y) coords.
xycoords :: Int -> (Int, Int)
xycoords 1 = (0,0)
xycoords n = do
  let (r,d) = ringfinder n -- get ring and modulus.
  let rside = 2*r+1
  let ddiv = div d (rside-1)
  let dmod = mod d (rside-1)
  case ddiv of 
    0 -> (r, -r+dmod+1)
    1 -> (r-dmod-1, -r+rside-1)
    2 -> (r-rside+1, r-dmod-1)
    3 -> (-r+dmod+1, r-rside+1)
    _ -> (100,100)

 
------------------------------

vsert :: (Int, Int) -> Map.Map (Int, Int) Int -> (Int, Map.Map (Int, Int) Int)
vsert p m = do  
  let x = fst p
  let y = snd p
  let v1 = case (Map.lookup (x+1,y) m) of Just v -> v
                                          _ -> 0
  let v2 = case (Map.lookup (x-1,y) m) of Just v -> v
                                          _ -> 0
  let v3 = case (Map.lookup (x,y+1) m) of Just v -> v
                                          _ -> 0
  let v4 = case (Map.lookup (x,y-1) m) of Just v -> v
                                          _ -> 0
  let v5 = case (Map.lookup (x+1,y+1) m) of Just v -> v
                                            _ -> 0
  let v6 = case (Map.lookup (x-1,y+1) m) of Just v -> v
                                            _ -> 0
  let v7 = case (Map.lookup (x+1,y-1) m) of Just v -> v
                                            _ -> 0
  let v8 = case (Map.lookup (x-1,y-1) m) of Just v -> v
                                            _ -> 0
  let rs = v1+v2+v3+v4+v5+v6+v7+v8
  (rs, Map.insert p (v1+v2+v3+v4+v5+v6+v7+v8) m)


tcheck :: Int -> [(Int, Int)] -> Map.Map (Int, Int) Int -> Int
tcheck n p m = do
  let (v, nm) = vsert (head p) m
  if (v > n) then v else tcheck n (tail p) nm

-- Magic 265149 is the input.
  
-- Since the upper routine is bounded, it's guaranteed to stop,
-- so we can use an infinite list to construct lst.
main = do  
  let lst = map xycoords iterate (\x -> x+1) 2
  let m = Map.singleton (0,0) 1
  putStrLn (show (tcheck 265149 lst m))
