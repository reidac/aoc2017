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

knothash :: String -> String
knothash s = do
  let fulllengths = lmultiplier 64 [] ((map (\x -> ord x) s) ++ [17,31,73,47,23])
  let starthash = [0..255]
  let densehash = densify [] (lprocess 0 fulllengths starthash)
  foldr (++) "" (map tohex densehash)
  
--------------------------------------

charbit :: Char -> Int
charbit '0' = 0
charbit '1' = 1
charbit '2' = 1
charbit '3' = 2
charbit '4' = 1
charbit '5' = 2
charbit '6' = 2
charbit '7' = 3
charbit '8' = 1
charbit '9' = 2
charbit 'a' = 2
charbit 'b' = 3
charbit 'c' = 2
charbit 'd' = 3
charbit 'e' = 3
charbit 'f' = 4

stringbit :: String -> Int
stringbit s = sum (map charbit s)

--------------------------------------
main = do
  let strs = [ "ffayrhll-" ++ (show x) | x <- [0..127] ]
  let khs = map knothash strs
  let res = sum (map stringbit khs)
  putStrLn (show res)

 
