

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

main = do 
  let (x,y) = xycoords 265149
  putStrLn (show (abs x + abs y))
 
