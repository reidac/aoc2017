import qualified Data.Map as Map
import qualified Data.Set as Set

data Operation = Inc String Int | Dec String Int deriving (Show)

data Condition = Gt String Int
                 | Lt String Int
                 | Ge String Int
                 | Le String Int
                 | Ee String Int
                 | Ne String Int deriving (Show)


-- Why do I need this?  This feels like I'm missing something about Haskell.
resolve :: Maybe Int -> Int
resolve x = case x of Just v -> v
                      _ -> 0

parse :: [String] -> Set.Set String -> [(Operation, Condition)] -> ( Set.Set String, [(Operation, Condition)] )
parse [] r p = (r,p)
parse l r p = do
  let wds = words (head l)
  let newr = Set.insert (head wds) r
  let newpe = ( case (wds !! 1) of "inc" -> Inc ( wds !! 0) (read (wds !! 2) :: Int)
                                   "dec" -> Dec ( wds !! 0) (read (wds !! 2) :: Int),
                case (wds !! 5) of ">" -> Gt (wds !! 4) (read (wds !! 6) :: Int )
                                   ">=" -> Ge (wds !! 4) (read (wds !! 6) :: Int)
                                   "<" -> Lt (wds !! 4) (read (wds !! 6) :: Int )
                                   "<=" -> Le (wds !! 4) (read (wds !! 6) :: Int )
                                   "==" -> Ee (wds !! 4) (read (wds !! 6) :: Int)
                                   "!=" -> Ne (wds !! 4) (read (wds !! 6) :: Int )
              )
  parse (tail l) newr (p ++ [newpe])



-- Evaluate a condition on a particular map.
ceval :: Condition -> Map.Map String Int -> Bool
ceval c m = case c of Gt s v -> (resolve (Map.lookup s m)) > v
                      Ge s v -> (resolve (Map.lookup s m)) >= v
                      Lt s v -> (resolve (Map.lookup s m)) < v
                      Le s v -> (resolve (Map.lookup s m)) <= v
                      Ee s v -> (resolve (Map.lookup s m)) == v
                      Ne s v -> (resolve (Map.lookup s m)) /= v
            

-- Perform an operation on a map.
-- Pattern matching should remove duplicate lookups?
perfop :: Operation -> Map.Map String Int -> Map.Map String Int
perfop (Inc s v) m = do
  let ov = resolve (Map.lookup s m)
  Map.insert s (ov+v) m
perfop (Dec s v) m = do
  let ov = resolve (Map.lookup s m)
  Map.insert s (ov-v) m

runpgm :: Map.Map String Int -> [(Operation, Condition)] -> Map.Map String Int
runpgm m [] = m
runpgm m p = do
  let (op,cn) = head p
  let nm = case (ceval cn m) of True -> (perfop op m)
                                False -> m
  runpgm nm (tail p)

main = do
  fdt <- readFile "day8.txt"
  let ops = lines fdt
  let regset = Set.empty
  let opseq = []
  let (regs,pgm) = parse ops regset opseq
  --
  let mp = Map.fromList (zip (Set.toList regs) (repeat 0))
  let nmp = runpgm mp pgm
  putStrLn (show (maximum (Map.elems nmp)))
                        
