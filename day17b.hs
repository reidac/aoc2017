
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

-- Insertion point, recursive definition.
ipt :: Int -> Int
ipt 0 = 0
ipt n = mod ((mipt (n-1)) + 1 + stride) n

-- Memoization trick.
mipt :: Int -> Int
mipt = (map ipt [0..] !!)

-- The actual solution is, what is the largest n less than or equal to
-- 50000000 for which the insertion point is zero?
-- Answer: 10150888
-- This function is weirdly slow, even with memoization. Python was
-- ~1000x faster?
main = do
  let lst = filter (\x -> (mipt x) == 0) [0..50000000]
  putStrLn (show lst)
