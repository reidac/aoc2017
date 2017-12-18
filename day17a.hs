
data CList = CElem { val :: Int,
                     next :: CList } deriving (Show)

-- stride = 3 -- Test input.
stride = 394 -- Puzzle input.

advance :: Int -> CList -> CList
advance 0 c = c
advance n (CElem a c) = advance (n-1) c

-- Reconstruct the list, consuming the second argument, until you get
-- to an element whose value is the same as the new head.  Replace
-- that one with the new passed-in head.
chain :: CList -> CList -> CList
chain newh tl = do
  if (val tl) == (val newh) then newh
    else (CElem (val tl) (chain newh (next tl)))
         
-- Insert a new item with key n after the current head.  Return
-- the current head, not the inserted item!
insert :: Int -> CList -> CList
insert n tl = do
  -- tl of length 1 is a special case.
  if (val tl) == (val (next tl)) then do
    let newh = CElem (val tl) (CElem n newh)
    newh
  -- tl has at least two elements.  Rebuild the tail.
  else do
    let newh = CElem (val tl) (CElem n (chain newh (next tl)))
    newh

-- The Algorithm.  Advance by stride, do the insertion.
-- The extra "advance 1" is because my insertion routine returns
-- the previous head, not the newly-inserted element.
rotinsert :: Int -> CList -> CList
rotinsert n c = advance 1 (insert n (advance stride c))

multinsert :: [Int] -> CList -> CList
multinsert [] c = c
multinsert (n:ns) c = multinsert ns (rotinsert n c)

-- The Answer.  Find the CElem whose int is n, and return the
-- value after it.
successor :: Int -> CList -> Int
successor n c = if (val c) == n then val (next c) else successor n (next c)

dump :: Int -> CList -> String -> String
dump 0 _ s = s
dump n c s = dump (n-1) (next c) (s++" "++(show (val c)))



main = do
  let sg0 = CElem 0 sg0
  --
  let sgx = multinsert [1..2017] sg0
  --
  putStrLn (show (successor 2017 sgx))
