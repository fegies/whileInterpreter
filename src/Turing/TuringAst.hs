module Turing.TuringAst where

data TuringMachine = TuringMachine {
    states :: [TuringState],
    inputAlphabet :: [TuringChar],
    bandAlphabet :: [TuringChar],
    transitions :: [(TuringState,TuringChar,TuringState,TuringChar,TuringDirection)],
    startState :: TuringState,
    finalStates :: [TuringState]
} deriving(Show)

class FromChar a where
    fromChar :: Char -> a

type TuringState = String
data TuringChar = EmptyChar | NonemptyChar Char deriving(Show)

instance Eq TuringChar where
    NonemptyChar l == NonemptyChar r = l == r
    EmptyChar == EmptyChar = True
    (==) _ _ = False

instance FromChar TuringChar where
    fromChar c = if c == '☐' then EmptyChar else NonemptyChar c

data TuringDirection
    = DirectionL
    | DirectionN
    | DirectionR
    deriving(Show)

instance FromChar TuringDirection where
    fromChar c = case c of
        'L' -> DirectionL
        'R' -> DirectionR
        _ -> DirectionN