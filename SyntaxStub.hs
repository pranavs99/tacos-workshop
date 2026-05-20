module Syntax where


--------------------------------------------------------------------------------
-- linguistic types and objects
--------------------------------------------------------------------------------

data Symbol =
    TP | DP | VP | D | N | V
    deriving (Show, Eq)

-- learning from previous mistakes and already using record syntax
data Node
    -- Branch Symbol [Node]
    = Branch {
        brSymOf     :: Symbol,
        childrenOf  :: [Node]
    }
    -- Leaf Symbol String
    | Leaf {
        leafSymOf   :: Symbol,
        wordOf      :: String
    }
    deriving (Show, Eq)

-- a free helper function to print out the words in a Node object :)
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
-- Branch-type Node objects should not return any words since you can't
-- translate internal tree structure itself
translateWord (Branch _ _) = []
-- Leaf-type Node objects should return aligned German word(s)
translateWord (Leaf leafSym word) = case (leafSym, word) of
    (N, "I")        -> ["ich"]
    (V, "forget")   -> ["vergesse"]
    (V, "forgot")   -> ["habe", "vergessen"]
    (D, "my")       -> ["mein"]
    (N, "phone")    -> ["Handy"]
    -- elsewhere case, could be useful for catching errors :)
    _ -> []

-- write a function "translateNode" that takes in:
--      an English tree as Node
-- and returns:
--      a German tree as Node
translateNode :: Node -> Node
translateNode n = undefined

-- a function that takes in:
--      an English tree as Node
-- and returns:
--      a translated German sentence as [String]
translate :: Node -> [String]
translate n = printWords (translateNode n)
