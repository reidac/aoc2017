import Data.String
import Data.Char

-- Remove garbage.  Garbage starts with <, and ends with
-- an unescaped >.  The boolean tells us if we're currently
-- inside garbage.  Read from i and accumulate o.
clean :: Bool -> String -> String -> String
clean False o "" = o
clean b o (i:is) = do
  case b of False -> case i of '<' -> clean True o is
                               _ -> clean False (o ++ [i]) is
            True -> case i of '!' -> clean True o (tail is)
                              '>' -> clean False o is
                              _ -> clean True o is

-- Inputs are the current level and the cumulative score.
score :: Int -> Int -> String -> Int
score l v "" = v
score l v (s:ss) = case s of '{' -> score (l+1) v ss
                             '}' -> score (l-1) (v+l) ss
                             ',' -> score l v ss


main = do
  fdat <- readFile "day9.txt"
  let str = head ( lines ( fdat ))
  let str2 = clean False "" str
  let res = score 0 0 str2
  putStrLn (show (res :: Int) ) 
