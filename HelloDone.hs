module Hello where


--------------------------------------------------------------------------------
-- 1. getting started
-- write a function called "square" that returns the squared value of an Int.
--------------------------------------------------------------------------------

square :: Int -> Int
square x = x * x


--------------------------------------------------------------------------------
-- 2. recursion
-- write a function called "factorial" that returns x! for a given Int x.
--------------------------------------------------------------------------------

factorial :: Int -> Int
factorial 0 = 1
factorial x = x * factorial (x - 1)

factorialGuard :: Int -> Int
factorialGuard x
    | x < 0     = error "(factorial) input cannot be negative"
    | x == 0    = 1
    | otherwise = x * factorial (x - 1)


--------------------------------------------------------------------------------
-- 3. map
-- write a function called "factorialList" that returns x! for a given LIST of
-- Ints xs.
--------------------------------------------------------------------------------

factorialList :: [Int] -> [Int]
factorialList [] = []
factorialList (x : x') =
    factorial x : factorialList x'

-- Use map
-- Found:
--   factorialList [] = []
-- factorialList (x : x') = factorial x : factorialList x'
-- Why not:
--   factorialList x' = map factorial x'

factorialMap :: [Int] -> [Int]
factorialMap xs = map factorial xs

factorialCurry :: [Int] -> [Int]
factorialCurry = map factorial


--------------------------------------------------------------------------------
-- 4. RecInt
-- 4.1) create a data type "RecInt" that recursively represents an Int.
-- 4.2) declare RecInt objects for 0, 1, 2, and 3.
-- 4.3) write a function "addRec" that returns a RecInt representing the sum
--      of two RecInt objects "x" and "y".
--------------------------------------------------------------------------------

data RecInt
    = Zero
    | Incr RecInt
    deriving (Show, Eq)

zero, one, two, three :: RecInt
zero    = Zero
one     = Incr Zero
two     = Incr one      -- Incr (Incr Zero)
three   = Incr two      -- Incr (Incr (Incr Zero))

addRecs :: RecInt -> RecInt -> RecInt
addRecs x y = case x of
    Zero    -> y
    Incr x' -> Incr (addRecs x' y)

rec2Int :: RecInt -> Int
rec2Int Zero        = 0
-- why doesn't this work?
-- rec2Int Incr x' = 1 + rec2Int x'
rec2Int (Incr x')   = 1 + rec2Int x'

add :: RecInt -> RecInt -> Int
add x y = rec2Int (addRecs x y)
