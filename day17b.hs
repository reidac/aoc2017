import qualified Data.Map as Map
-- Part b is all math.

-- Our stride is 394.
-- After step n, we have done an insertion.  The insertion took
-- place at a point which is advanced by the stride from the previous
-- insertion point, the value that gets inserted is just n,
-- and the new current point is the successor of the insertion point.
--
-- So v_n = n, the insertion point i_n is c_(n-1) + (mod stride n).
-- The new current point, c_n, is (i_n+1).  No modulus, the list is
-- extended if required.

stride = 394


-- Manual memoization.  Ship the dictionary around.
inner :: (Int, Map.Map Int Int) -> (Int, Map.Map Int Int)
inner (0,m) = (0,m)
inner (n,m) = do
  let (nn,nm) = outer (n-1,m)
  ((rem (nn + 1 + stride) n),nm)

outer :: (Int, Map.Map Int Int) -> (Int, Map.Map Int Int)
outer (n, m) = if (Map.member n m) then ((Map.findWithDefault 0 n m), m) else do
  let (r, nm) = inner (n, m) 
  (r, Map.insert n r nm)


build :: Int -> [(Int,Int)] -> Map.Map Int Int -> [(Int,Int)]
build 0 l m = l
build n l m = do
  let (r,nm) = outer (n,m)
  if r==0 then (build (n-1) (l++[(n,r)]) nm) else (build (n-1) l nm)

-- The actual solution is, what is the largest n less than or equal to
-- 50000000 for which the insertion point is zero?
-- Answer: 10150888
-- This manually-memoized version matches Python for speed.  Don't
main = do
  let m = Map.empty
  let r = build 50000000 [] m
  putStrLn (show (map fst r))
