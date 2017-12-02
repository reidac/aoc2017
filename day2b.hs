import Data.Char
import Data.String
import Data.List


imax :: [Int] -> Int
imax [] = 0
imax (x:xs) = max x (imax xs) 

imin :: [Int] -> Int
imin [] = 9999999
imin (x:xs) = min x (imin xs)

-- Don't understand why this can't be a lambda, but I get parse errors.
imap :: String -> Int
imap x = read x :: Int

-- For every distinct pair of ints in the list which divide
-- evenly, add up the divisions, and return them.
 
-- Utility function, do the division if possible.
divres :: (Int, Int) -> Int
divres a = do 
  let v1 = if (mod (fst a) (snd a) ) == 0 then (div (fst a) (snd a) ) else 0
  let v2 = if (mod (snd a) (fst a) ) == 0 then (div (snd a) (fst a) ) else 0
  v1+v2


pairs :: [Int] -> [(Int,Int)]
pairs a = [ (x,y) | (x:ys) <- tails a, y <- ys ]

intify :: String -> Int
intify a = do 
  let ilst = map imap ( words a ) 
  sum (map divres (pairs ilst))


main = do
  fct <- readFile "day2.txt"
  let lns = lines fct
  let ckdat = map intify lns
  putStrLn (show (sum ckdat))
