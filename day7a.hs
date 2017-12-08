import Data.List
import Data.String

consume :: [String] -> [String] -> [String] -> ([String],[String])
consume [] p c = (p, c)
consume lns p c = do
  let wds = words (head lns)
  let pnew = p ++ [head(wds)]
  let cnew = if ((length wds) > 3) then (c ++ (drop 3 wds) )
                                   else c
  consume (tail lns) pnew cnew                                  

-- The root of the tree is the string that only occurs in the
-- parent list, and not in the child list.  Assume it's unique,
-- avoid having to build the tree or muck about with weights.
main = do
  let parents = []
  let children = []
  fdat <- readFile "day7c.txt" -- Version with commas removed.
  let lns = lines fdat
  let res = consume lns parents children
  let loner = (fst res) \\ (snd res)
  putStrLn (show loner)
