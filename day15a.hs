
astart = 618
bstart = 814

afactor = 16807
bfactor = 48271

prng :: Integer -> Integer -> Integer
prng p f = mod (p*f) 2147483647

-- Count how many times the a and b gens are equal.
eqcheck :: Integer -> Integer -> Integer -> Integer -> Integer
eqcheck 0 r _ _ = r
eqcheck n r ap bp = do
  let an = prng ap afactor
  let bn = prng bp bfactor
  if ((mod an 65536) == (mod bn 65536)) then eqcheck (n-1) (r+1) an bn
    else eqcheck (n-1) r an bn

main = do
  let res = eqcheck 40000000 0 astart bstart
  putStrLn (show res)

