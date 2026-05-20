module Phonology where


--------------------------------------------------------------------------------
-- linguistic features and objects
--------------------------------------------------------------------------------

data FrontnessValue =
    Front | Neutral | Back
    deriving (Show, Eq)

data CaseValue =
    Inessive | Elative
    deriving (Show, Eq)

data Vowel =
    Vowel Char FrontnessValue
    deriving (Show, Eq)

--------------------------------------------------------------------------------
-- Finnish vowel inventory and functions
--------------------------------------------------------------------------------

finnishVowels :: [Vowel]
finnishVowels = [
    Vowel 'A' Front,
    Vowel 'O' Front,
    Vowel 'y' Front,
    Vowel 'e' Neutral,
    Vowel 'i' Neutral,
    Vowel 'a' Back,
    Vowel 'o' Back,
    Vowel 'u' Back]

-- 1) detect WHETHER a given Char is a Finnish vowel
isFinnishVowel :: Char -> Bool
isFinnishVowel c = undefined

-- 2) lookup FrontnessValue from a given Char's corresponding Vowel object
--
-- *** why might we want to return [FrontnessValue] and not just FrontnessValue?
--
lookupFinnishFrontness :: Char -> [FrontnessValue]
lookupFinnishFrontness c = undefined

--------------------------------------------------------------------------------
-- conjugation and harmony
--------------------------------------------------------------------------------

-- 3) tie this all together in a "conjugate" function that takes in:
--      a Finnish word as String
--      the intended case as CaseValue
-- and returns:
--      (word as String, harmonized affix as String)
conjugate :: String -> CaseValue -> (String, String)
conjugate word cv = undefined
