import Data.Char
import Data.String
import qualified Data.Map as Map
import Data.List

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

-- Create a particle map from the input strings.
particulate :: [[String]] -> Int -> Map.Map Int Particle -> Map.Map Int Particle
particulate [] _ m = m
particulate (l:ls) n m = do
  let [px,py,pz] = map (\x -> read x :: Int) (words ( decomma (l !! 0) ""))
  let [vx,vy,vz] = map (\x -> read x :: Int) (words ( decomma (l !! 1) ""))
  let [ax,ay,az] = map (\x -> read x :: Int) (words ( decomma (l !! 2) ""))
  particulate ls (n+1) (Map.insert n (Ptcl (px,py,pz) (vx,vy,vz) (ax,ay,az)) m)


-- Move a particle forward in time.
padvance :: Particle -> Particle
padvance p = do
  let (px,py,pz) = pos p
  let (vx,vy,vz) = vel p
  let (ax,ay,az) = acc p
  Ptcl (px+vx+ax,py+vy+ay,pz+vz+az) (vx+ax,vy+ay,vz+az) (ax,ay,az)

-- Remove particles whose positions coincide.
decollide :: Map.Map Int Particle -> Map.Map Int Particle
decollide m = do
  let pairs = [(p1,p2) | (p1:ps) <- tails (Map.toList m), p2 <- ps]
  foldr (\((k1,p1),(k2,p2)) pm -> if ((pos p1)==(pos p2)) then
            (Map.delete k1 (Map.delete k2 pm)) else pm ) m pairs

-- Iterate up to N and return the size of the modified map.
countafter :: Int -> Map.Map Int Particle -> Int
countafter 0 m = Map.size m
countafter n m = countafter (n-1) (decollide (Map.map padvance m))

main = do
  fdat <- readFile "day20.txt"
  let pstr = map (\x -> brackpass x []) (lines fdat)
  let pmap = particulate pstr 0 Map.empty
  let res = countafter 200 pmap
  putStrLn (show res)
