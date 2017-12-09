import Data.String
import Data.Char

-- Remove garbage.  Garbage starts with <, and ends with
-- an unescaped >.  The boolean tells us if we're currently
-- inside garbage.  Read from i and accumulate o.
-- Part B: Count up what you see.
clean :: Bool -> Int -> String -> String -> (Int, String)
clean False c o "" = (c,o)
clean b c o (i:is) = do
  case b of False -> case i of '<' -> clean True c o is
                               _ -> clean False c (o ++ [i]) is
            True -> case i of '!' -> clean True c o (tail is)
                              '>' -> clean False c o is
                              _ -> clean True (c+1) o is

-- Inputs are the current level and the cumulative score.
score :: Int -> Int -> String -> Int
score l v "" = v
score l v (s:ss) = case s of '{' -> score (l+1) v ss
                             '}' -> score (l-1) (v+l) ss
                             ',' -> score l v ss


main = do
  fdat <- readFile "day9.txt"
  let str = head ( lines ( fdat ))
  let (ct,str2) = clean False 0 "" str
  -- let res = score 0 0 str2
  putStrLn (show (ct :: Int) ) 
