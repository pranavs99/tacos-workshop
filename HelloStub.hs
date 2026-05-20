module Hello where


--------------------------------------------------------------------------------
-- 1. getting started
-- write a function called "square" that returns the squared value of an Int.
--------------------------------------------------------------------------------

square :: Int -> Int
square x = undefined


--------------------------------------------------------------------------------
-- 2. recursion
-- write a function called "factorial" that returns x! for a given Int x.
--------------------------------------------------------------------------------

factorial :: Int -> Int
factorial x = undefined

-- syntactic sugar: use "guards"
factorialGuard :: Int -> Int
factorialGuard x = undefined


--------------------------------------------------------------------------------
-- 3. map
-- write a function called "factorialList" that returns x! for a given LIST of
-- Ints xs.
--------------------------------------------------------------------------------

factorialList :: [Int] -> [Int]
factorialList xs = undefined

-- remember map() from Python? it's a Haskell original... try it out here:
factorialMap :: [Int] -> [Int]
factorialMap xs = undefined

-- we actually don't need "xs" in "factorialMap"...
-- look up "currying" on Wikipedia :)
factorialCurry :: [Int] -> [Int]
factorialCurry = undefined


--------------------------------------------------------------------------------
-- 4. RecInt
-- 4.1) create a data type "RecInt" that recursively represents an Int.
-- 4.2) declare RecInt objects for 0, 1, 2, and 3.
-- 4.3) write a function "addRec" that returns a RecInt representing the sum
--      of two RecInt objects "x" and "y".
--------------------------------------------------------------------------------

-- 4.1) already done for you!
data RecInt
    = Zero
    | Incr RecInt
    deriving (Show, Eq)

-- 4.2) implement the "recursive" versions of the listed integers
zero, one, two, three :: RecInt
zero    = undefined
one     = undefined
two     = undefined
three   = undefined

-- 4.3) how are we going to add together integers of this structure?
--
-- think about two things:
--      1) what structure can x and y have?
--      2) if we're recursing, how can we keep building our number?
addRecs :: RecInt -> RecInt -> RecInt
addRecs x y = undefined

-- to prove that this stuff works, write a function "rec2Int" that converts
-- a RecInt back to a regular Int
rec2Int :: RecInt -> Int
rec2Int r = undefined

-- and to tie everything together, write a function "add" that returns an
-- actual Int, when given two RecInt objects
add :: RecInt -> RecInt -> Int
add x y = undefined
