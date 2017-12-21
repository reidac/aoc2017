import Data.Char
import Data.String
import qualified Data.Map as Map

data Particle = Ptcl { pos :: (Int, Int, Int),
                       vel :: (Int, Int, Int),
                       acc :: (Int, Int, Int) } deriving (Show)

decomma :: String -> String -> String
decomma "" s = s
decomma (c:cs) s = case c of ',' -> decomma cs (s++[' '])
                             _   -> decomma cs (s++[c])
                           
-- Only call this when you're inside the brackets.
brackload :: String -> String -> [String] -> [String]
brackload (c:cs) wrk lst = case c of
                             '>' -> brackpass cs (lst++[wrk])
                             _   -> brackload cs (wrk++[c]) lst

-- Call this one when you're outside the brackets.
brackpass :: String -> [String] -> [String]
brackpass "" lst = lst
brackpass (c:cs) lst = case c of
                         '<' -> brackload cs "" lst
                         _   -> brackpass cs lst


particulate :: [[String]] -> Int -> Map.Map Int Particle -> Map.Map Int Particle
particulate [] _ m = m
particulate (l:ls) n m = do
  let [px,py,pz] = map (\x -> read x :: Int) (words ( decomma (l !! 0) ""))
  let [vx,vy,vz] = map (\x -> read x :: Int) (words ( decomma (l !! 1) ""))
  let [ax,ay,az] = map (\x -> read x :: Int) (words ( decomma (l !! 2) ""))
  particulate ls (n+1) (Map.insert n (Ptcl (px,py,pz) (vx,vy,vz) (ax,ay,az)) m)

-- Find the index of the particle with the smallest "Manhattan acceleration".
lowesta :: [(Int, Particle)] -> (Int, Int) -> (Int, Int)
lowesta [] a = a
lowesta (p:ps) a = do
  let idx = (fst p)
  let (ax,ay,az) = acc (snd p)
  let aval = (abs ax) + (abs ay) + (abs az)
  if (aval < (snd a)) then lowesta ps (idx,aval) else lowesta ps a
  
main = do
  fdat <- readFile "day20.txt"
  let pstr = map (\x -> brackpass x []) (lines fdat)
  let pmap = particulate pstr 0 Map.empty
  let (lowdx,lowval) = lowesta (Map.toList pmap) (-1,100000)
  putStrLn (show lowdx)
