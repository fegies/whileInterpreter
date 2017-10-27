module While.WhileAst where

data Ast
    = Assign Variable Variable Operator Constant
    | Loop Variable Ast
    | While Variable Ast
    | Sequence Ast Ast
    deriving(Show)

type Variable = String
type Constant = Integer

data Operator
    = Plus
    | Minus
    deriving(Show)