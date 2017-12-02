import Data.Char
import Data.String


imax :: [Int] -> Int
imax [] = 0
imax (x:xs) = max x (imax xs) 

imin :: [Int] -> Int
imin [] = 9999999
imin (x:xs) = min x (imin xs)

-- Don't understand why this can't be a lambda, but I get parse errors.
imap :: String -> Int
imap x = read x :: Int

intify :: String -> Int
intify a = do 
  let ilst = map imap ( words a ) 
  (imax ilst) - (imin ilst)

main = do
  fct <- readFile "day2.txt"
  let lns = lines fct
  -- putStrLn (show lns)
  let ckdat = map intify lns
  putStrLn (show (sum ckdat))
