import qualified Data.Map as Map

rotate2 :: String -> String
rotate2 [ul,ur,_,ll,lr] = [ll,ul,'/',lr,ur]

flip2 :: String -> String
flip2 [ul,ur,_,ll,lr] = [ur,ul,'/',lr,ll]

rotate3 :: String -> String
rotate3 [xx,xy,xz,_,yx,yy,yz,_,zx,zy,zz] = [zx,yx,xx,'/',zy,yy,xy,'/',zz,yz,xz]

flip3 :: String -> String
flip3 [xx,xy,xz,_,yx,yy,yz,_,zx,zy,zz] = [xz,xy,xx,'/',yz,yy,yx,'/',zz,zy,zx]

rptn :: String -> String
rptn s = if ((length s) == 5) then (rotate2 s) else (rotate3 s)

fptn :: String -> String
fptn s = if ((length s) == 5) then (flip2 s) else (flip3 s)


-- The full set of possible transformations of any string is
-- the original and all the rotations, and the flipped original
-- and all the flipped rotations.  For high-symmetry figures, there
-- will be redundancies, but they'll just overwrite with the right
-- answer, so it's OK.

-- Also, there's probably a more Haskell-y way to generate powers
-- of the rotation operation.  Scanr?
mappify :: [String] -> Map.Map String String -> Map.Map String String
mappify [] m = m
mappify (l:ls) m = do
  let [ptn,_,trns] = words l
  let nm = foldr (\x mx -> Map.insert x trns mx) m
           [ptn, rptn ptn, rptn (rptn ptn),
             rptn (rptn (rptn ptn)),
             fptn ptn, rptn (fptn ptn),
             rptn (rptn (fptn ptn)),
             rptn (rptn (rptn (fptn ptn)))]
  mappify ls nm

  

main = do
  fdat <- readFile "day21.txt"
  let tm = mappify (lines fdat) Map.empty
  putStrLn (show tm)
