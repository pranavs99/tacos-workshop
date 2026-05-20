module Syntax where


--------------------------------------------------------------------------------
-- linguistic types and objects
--------------------------------------------------------------------------------

data Symbol
    = TP | DP | VP
    | D | N | V
    deriving (Show, Eq)

data Node
    = Branch {
        brSymOf     :: Symbol,
        childrenOf  :: [Node]
    }
    | Leaf {
        leafSymOf   :: Symbol,
        wordOf      :: String
    }
    deriving (Show, Eq)

printWords :: Node -> [String]
printWords (Leaf leafSym word) =
    ["(" ++ show leafSym ++ ") " ++ word]
printWords (Branch brSym children) =
    concatMap printWords children

--------------------------------------------------------------------------------
-- sentences as Node objects
--------------------------------------------------------------------------------

-- "I forgEt my phone"
n1 :: Node
n1 =
    Branch TP [
        Branch DP [
            Leaf N "I"
        ],
        Branch VP [
            Leaf V "forget",
            Branch DP [
                Leaf D "my",
                Leaf N "phone"
            ]
        ]
    ]

-- "I forgOt my phone"
n2 :: Node
n2 =
    Branch TP [
        Branch DP [
            Leaf N "I"
        ],
        Branch VP [
            Leaf V "forgot",
            Branch DP [
                Leaf D "my",
                Leaf N "phone"
            ]
        ]
    ]

--------------------------------------------------------------------------------
-- translator
--------------------------------------------------------------------------------

-- word-level translation function that takes in:
--      a tree (or subtree) as Node
-- and returns:
--      the English -> German aligned words as [String]
--
-- *** why might we want to return [String] and not just String?
--
translateWord :: Node -> [String]
translateWord (Branch _ _) = []
translateWord (Leaf leafSym word) = case (leafSym, word) of
    (N, "I")        -> ["ich"]
    (V, "forget")   -> ["vergesse"]
    (V, "forgot")   -> ["habe", "vergessen"]
    (D, "my")       -> ["mein"]
    (N, "phone")    -> ["Handy"]
    -- elsewhere case
    _ -> []

-- write a function "translateNode" that takes in:
--      an English tree as Node
-- and returns:
--      a German tree as Node
translateNode :: Node -> Node
-- TP -> DP VP
translateNode (Branch TP [dp, vp]) =
    Branch TP [translateNode dp, translateNode vp]
-- DP -> headless N must be a pronoun
translateNode (Branch DP [engProLeaf]) =
    case translateWord engProLeaf of
        [deuPro] ->
            Branch DP [
                Leaf N deuPro
            ]
        _ ->
            error (
                "(translateWord) D-less NP not supported: "
                ++ wordOf engProLeaf)
-- DP -> D N
translateNode (Branch DP [engDLeaf, engNLeaf]) =
    case (translateWord engDLeaf, translateWord engNLeaf) of
        ([deuD], [deuN]) ->
            Branch DP [
                Leaf D deuD,
                Leaf N deuN
            ]
        _ ->
            error (
                "(translateNode) DP -> D N could not be translated:\n"
                ++ "D: " ++ wordOf engDLeaf ++ "\n"
                ++ "N: " ++ wordOf engNLeaf)
-- VP translation
translateNode (Branch VP [engVLeaf, engVComplement]) =
    case translateWord engVLeaf of
        -- modal V triggers V2 movement
        [deuAuxiliary, deuParticiple] ->
            Branch VP [
                Leaf V deuAuxiliary,
                Branch VP [
                    translateNode engVComplement,
                    Leaf V deuParticiple
                ]
            ]
        -- normal old V -> V translation, no effect on Node structure
        [deuMatrix] ->
            Branch VP [
                Leaf V deuMatrix,
                translateNode engVComplement
            ]
        _ ->
            error "(translateNode) unrecognized VP structure"
-- elsewhere case: for this workshop, just throw (error)
translateNode _ =
    error "(translateNode) unrecognized Node structure"

-- a function (translate) that takes in:
--      an English tree as Node
-- and returns:
--      a translated German sentence as [String]
translate :: Node -> [String]
translate n = printWords (translateNode n)
