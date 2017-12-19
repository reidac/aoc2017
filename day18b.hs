import Data.String
import Data.Char
import Data.List
import qualified Data.Map as Map


-- Generate an integer-indexed map of the instructions.
-- Indexing allows the jump instruction to work.
parse :: Int -> [String] -> Map.Map Int [String] -> Map.Map Int [String]
parse _ [] m = m
parse n (l:ls) m = do
  let wds = words l
  parse (n+1) ls (Map.insert n wds m)

-- Resolve the string s into either a register value or a direct integer.
regget :: String -> Map.Map String Int -> Int
regget s m = do
  if (isLetter (head s)) then (Map.findWithDefault 0 s m)
    else (read s :: Int)

-- Set register s to the value v, creating it if required.
-- Map insertion semantics do this pretty much automatically.
regset :: String -> Int -> Map.Map String Int -> Map.Map String Int
regset s v m = Map.insert s v m


-- Step a program state forward.  Consume data from the head of the
-- "in" list, if possible, and write data to the end of the
-- "out" list, as required.  Caller promises that pc is in the map.
step :: (Int, Map.Map String Int, [Int], [Int])
     -> Map.Map Int [String] -> (Int, Map.Map String Int, [Int], [Int])
step (pc, regs, inlist, outlist) pgm = do
  let inst = Map.findWithDefault [] pc pgm
  case (head inst) of "snd" -> do
                        let v = (regget (inst !! 1) regs)
                        ((pc+1), regs, inlist, (outlist++[v]))
                      "set" -> do
                        let v = (regget (inst !! 2) regs)
                        ((pc+1), (regset (inst !! 1) v regs), inlist, outlist)
                      "add" -> do
                        let v1 = (regget (inst !! 1) regs)
                        let v2 = (regget (inst !! 2) regs)
                        ((pc+1), (regset (inst !! 1) (v1+v2) regs), inlist, outlist)
                      "mul" -> do
                        let v1 = (regget (inst !! 1) regs)
                        let v2 = (regget (inst !! 2) regs)
                        ((pc+1), (regset (inst !! 1) (v1*v2) regs), inlist, outlist)
                      "mod" -> do
                        let v1 = (regget (inst !! 1) regs)
                        let v2 = (regget (inst !! 2) regs)
                        ((pc+1), (regset (inst !! 1) (mod v1 v2) regs), inlist, outlist)
                      "rcv" -> do
                        ((pc+1), (regset (inst !! 1) (head inlist) regs), (tail inlist), outlist)
                      "jgz" -> do
                        let v1 = (regget (inst !! 1) regs)
                        let v2 = (regget (inst !! 2) regs)
                        if (v1>0) then ((pc+v2), regs, inlist, outlist)
                          else ((pc+1), regs, inlist, outlist)

-- Can the program state be run with the instruction set?
runnable :: (Int, Map.Map String Int, [Int], [Int]) -> Map.Map Int [String] -> Bool
runnable (pc, regs, inlist, outlist) pgm = do
  if not (Map.member pc pgm) then False else
    if ((((Map.findWithDefault [] pc pgm) !! 0) == "rcv") && (null inlist)) then False else
      True

-- Is this program about to run a "snd" instruction?
sending :: (Int, Map.Map String Int, [Int], [Int]) -> Map.Map Int [String] -> Bool
sending (pc, regs, inlist, outlist) pgm = ((Map.findWithDefault [] pc pgm) !! 0) == "snd"


-- Advance the program states, and count up now many times program 1
-- sends a value -- this is the puzzle output.  Return this once
-- neither program is runnable.  Swap the lists around as appropriate.
run :: (Int, Map.Map String Int, [Int], [Int])
  -> (Int, Map.Map String Int, [Int], [Int])
  -> Map.Map Int [String] -> Int -> Int
run p0s p1s pgm ct = do
  if (runnable p0s pgm) then do
    let (p0pc, p0regs, p0in, p0out) = step p0s pgm
    let p0new = (p0pc, p0regs, p0in, p0out)
    let (p1pc, p1regs, p1in, p1out) = p1s
    let p1new = (p1pc, p1regs, p0out, p0in)
    run p0new p1new pgm ct
  else if (runnable p1s pgm) then do
    let nct = if (sending p1s pgm) then (ct+1) else ct  
    let (p1pc, p1regs, p1in, p1out) = step p1s pgm
    let p1new = (p1pc, p1regs, p1in, p1out)
    let (p0pc, p0regs, p0in, p0out) = p0s
    let p0new = (p0pc, p0regs, p1out, p1in)
    run p0new p1new pgm nct
  else -- Neither program is runnable.
    ct

main = do
  fdat <- readFile "day18.txt"
  let pgm = parse 0 (lines fdat) Map.empty
  let p0state = (0, (Map.fromList [("p",0)]), [], [])
  let p1state = (0, (Map.fromList [("p",1)]), [], [])
  putStrLn (show (run p0state p1state pgm 0))
