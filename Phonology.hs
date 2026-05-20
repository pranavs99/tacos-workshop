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

-- "record syntax"
data Vowel =
    Vowel {
        soundOf     :: Char,
        frontnessOf :: FrontnessValue
    } deriving (Show, Eq)
    -- Vowel Char Frontness
    -- soundOf (Vowel 'x' Neutral) = 'x'

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

isFinnishVowel :: Char -> Bool
isFinnishVowel c =
    elem c (map soundOf finnishVowels)

lookupFrontness :: [Vowel] -> Char -> [FrontnessValue]
-- try returning JUST Frontness (no list)
-- then bring up side effects and minimizing errors
lookupFrontness [] _ = []
lookupFrontness (v : v') c =
    if soundOf v == c then
        [frontnessOf v]
    else
        lookupFrontness v' c

lookupFinnishFrontness :: Char -> [FrontnessValue]
lookupFinnishFrontness = lookupFrontness finnishVowels


--------------------------------------------------------------------------------
-- conjugation and harmony
--------------------------------------------------------------------------------

extractFrontnessValues :: String -> [FrontnessValue]
extractFrontnessValues word =
    -- 1) find vowels in word
    let vowels = filter isFinnishVowel word in
    -- 2) assemble list of extracted FrontnessValue objects
    concatMap lookupFinnishFrontness vowels

extractFrontnessValuesClean :: String -> [FrontnessValue]
extractFrontnessValuesClean word =
    concatMap lookupFinnishFrontness (filter isFinnishVowel word)

shouldHarmonize :: String -> Bool
shouldHarmonize word =
    elem Back (extractFrontnessValues word)

conjugate :: String -> CaseValue -> (String, String)
conjugate word Inessive =
    if shouldHarmonize word then
        (word, "-ssa")
    else
        (word, "-ssA")
conjugate word Elative =
    if shouldHarmonize word then
        (word, "-sta")
    else
        (word, "-stA")

conjugateClean :: String -> CaseValue -> (String, String)
conjugateClean word cse =
    case (shouldHarmonize word, cse) of
        (True,  Inessive)   -> (word, "-ssa")
        (False, Inessive)   -> (word, "-ssA")
        (True,  Elative)    -> (word, "-sta")
        (False, Elative)    -> (word, "-stA")

-- maa-ssa
-- minA asu-n tA-ssA maa-ssa
--
-- seinA-ssA
-- televisio on seinA-ssA
-- metsA-ssA
-- me kAvelemme metsA-ssA
-- suome-ssa
-- tA-ssA
-- tuo-ssa
--
-- suome-sta
-- minA ole-n suome-sta
-- minA muista-n tA-stA pingviini-stA
-- minA muista-n tuo-sta pingviini-stA
-- minA
