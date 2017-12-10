import Data.String
import Data.Char
import Data.Bits

lrotate :: Int -> [Int] -> [Int]
lrotate v l = (drop v l) ++ (take v l)

lprocess :: Int -> [Int] -> [Int] -> [Int]
lprocess sk [] d = d
lprocess sk lg d = do
  let lgn = (head lg)
  let rt = mod (lgn + sk) (length d)
  let nl = ((reverse (take lgn d)) ++ (drop lgn d))
  let pnl = (lrotate rt nl)
  let qnl = (lprocess (sk+1) (tail lg) pnl)
  lrotate ((length qnl) - rt) qnl 
  
-- List multiplier.  lmultiplier n [] lst returns n copies of lst.
lmultiplier :: Int -> [Int] -> [Int] -> [Int]
lmultiplier 0 lr li = lr
lmultiplier n lr li = lmultiplier (n-1) (lr ++ li) li

-- Create the dense hash from the sparse hash.  Haskell folds!!
densify :: [Int] -> [Int] -> [Int]
densify d [] = d
densify d s = densify ( d ++ [(foldr xor 0 (take 16 s))]) (drop 16 s)

-- Hex conversion utilties.  Haskell maybe has these?
tohexd :: Int -> Char
tohexd v = case (v<10) of True -> chr(v+ord('0'))
                          False -> chr((v-10)+ord('a'))
                  
tohex :: Int -> String
tohex a = [tohexd (div a 16)] ++ [tohexd (mod a 16)]

main = do
  fdat <- readFile "day10.txt" -- Original version.
  let flengths = map (\x -> ord x) (head (lines fdat))
  let alllengths = flengths ++ [17,31,73,47,23]
  --
  let fulllengths = lmultiplier 64 [] alllengths
  --
  let starthash = [0..255]
  let sparsehash = lprocess 0 fulllengths starthash
  --
  let densehash = densify [] sparsehash
  let strhash = map tohex densehash
  let strng = foldr (++) "" strhash
  putStrLn (show strng)
 
