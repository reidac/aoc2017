import Data.String
import Data.Char
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

-- The instruction map doesn't change, still need to pass it through?
-- Book sez I can hide it with a Monad.  Time to learn about Monads?
-- PC, registers, instructions.  Return a string, I guess?
run :: Int -> Map.Map String Int -> Map.Map Int [String] -> String
run pc regs pgm = do
  if not (Map.member pc pgm) then "Halted!"
    else do
    let inst = Map.findWithDefault [] pc pgm
    case (head inst) of "snd" -> do
                          let v = (regget (inst !! 1) regs)
                          run (pc+1) (regset "snd" v regs) pgm
                        "set" -> do
                          let v = (regget (inst !! 2) regs)
                          run (pc+1) (regset (inst !! 1) v regs) pgm
                        "add" -> do
                          let v1 = (regget (inst !! 1) regs)
                          let v2 = (regget (inst !! 2) regs)
                          run (pc+1) (regset (inst !! 1) (v1+v2) regs) pgm
                        "mul" -> do
                          let v1 = (regget (inst !! 1) regs)
                          let v2 = (regget (inst !! 2) regs)
                          run (pc+1) (regset (inst !! 1) (v1*v2) regs) pgm
                        "mod" -> do
                          let v1 = (regget (inst !! 1) regs)
                          let v2 = (regget (inst !! 2) regs)
                          run (pc+1) (regset (inst !! 1) (mod v1 v2) regs) pgm
                        "rcv" -> do
                          let v1 = (regget "snd" regs)
                          if (v1>0) then ("Got "++(show v1))
                            else run (pc+1) regs pgm
                        "jgz" -> do
                          let v1 = (regget (inst !! 1) regs)
                          let v2 = (regget (inst !! 2) regs)
                          if (v1>0) then run (pc+v2) regs pgm
                            else run (pc+1) regs pgm
  
main = do
  fdat <- readFile "day18.txt"
  let pgm = parse 0 (lines fdat) Map.empty
  let regs = Map.empty
  putStrLn (run 0 regs pgm)
