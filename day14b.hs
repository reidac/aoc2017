import Data.String
import Data.Char
import Data.Bits

import qualified Data.Set as Set

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

charbit :: Char -> String
charbit '0' = "0000"
charbit '1' = "0001"
charbit '2' = "0010"
charbit '3' = "0011"
charbit '4' = "0100"
charbit '5' = "0101"
charbit '6' = "0110"
charbit '7' = "0111"
charbit '8' = "1000"
charbit '9' = "1001"
charbit 'a' = "1010"
charbit 'b' = "1011"
charbit 'c' = "1100"
charbit 'd' = "1101"
charbit 'e' = "1110"
charbit 'f' = "1111"


-- Convert the array into a set of (x,y) pairs.
setcols :: Int -> Int -> String -> Set.Set (Int,Int) -> Set.Set (Int,Int)
setcols _ _ "" t = t
setcols c r (s:ss) t = case s of '1' -> setcols (c+1) r ss (Set.insert (r,c) t)
                                 '0' -> setcols (c+1) r ss t

setrows :: Int -> [String] -> Set.Set (Int,Int) -> Set.Set (Int,Int)
setrows _ [] t = t
setrows r (s:ss) t = Set.union (setcols 0 r s t) (setrows (r+1) ss t)

-- Helper function, removes pairs adjacent to the input pair.
rmtravel :: (Int,Int) -> Set.Set (Int,Int) -> Set.Set (Int,Int)
rmtravel (r,c) s = do
  let s2 = Set.delete (r,c) s
  let nbrs = filter (\x -> Set.member x s2) [(r+1,c),(r-1,c),(r,c+1),(r,c-1)]
  if (length nbrs) == 0 then s2 else (foldr rmtravel s2 nbrs)

  
  
delgrp :: Int -> Set.Set (Int,Int) -> Int
delgrp l s = if (Set.null s) then l else do
  let stpt = head (Set.toList s)
  let s2 = rmtravel stpt s
  delgrp (l+1) s2

--------------------------------------
main = do
  let khs = map knothash [ "ffayrhll-" ++ (show x) | x <- [0..127] ]
  -- let khs = map knothash [ "flqrgnkx-" ++ (show x) | x <- [0..127] ]
  -- khs is a list of knot hashes in hex.  Convert to bits.
  let khbts = [ foldr (++) "" (map (\x -> charbit x) y) | y <- khs ]
  let oneset = setrows 0 khbts Set.empty
  --
  let res = delgrp 0 oneset
  --
  putStrLn (show res)

 
