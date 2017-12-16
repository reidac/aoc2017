import Data.Bits


astart = 65
bstart = 8921

afactor = 16807
bfactor = 48271

prng :: Int -> Int -> Int
prng p f = mod (p*f) 2147483647

-- Count how many times the a and b gens are equal.
eqcheck :: Int -> Int -> Int -> Int -> Int
eqcheck n r _ _ = r
eqcheck n r ap bp = do
  let an = prng ap afactor
  let bn = prng bp bfactor
  if ((an .&. 32767) == (bn .&. 32767)) then eqcheck (n-1) (r+1) an bn else eqcheck (n-1) r an bn

main = do
  let res = eqcheck 5 0 astart bstart
  putStrLn (show res)
