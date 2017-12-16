
-- astart = 65
-- bstart = 8921

astart = 618
bstart = 814

afactor = 16807
bfactor = 48271

prng :: Integer -> Integer -> Integer
prng p f = mod (p*f) 2147483647

-- Count how many times the a and b gens are equal.
eqcheck :: Integer -> Integer -> [Integer] -> [Integer] -> Integer
eqcheck 0 r _ _ = r
eqcheck n r (a:as) (b:bs) = do
  if ((mod a 65536) == (mod b 65536)) then eqcheck (n-1) (r+1) as bs
    else eqcheck (n-1) r as bs

main = do
  let res = (eqcheck 5000000 0
             (filter (\x -> (mod x 4)==0)
              (iterate (\x -> prng x afactor) astart))
             (filter (\x -> (mod x 8)==0)
              (iterate (\x -> prng x bfactor) bstart)) )
  putStrLn (show res)
