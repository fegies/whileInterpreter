module Interpreter(interpret) where

import WhileAst

type State = [(Integer,Integer)]

getVal :: State -> Integer -> Integer
getVal ((a,b):xs) s = if a == s then b else getVal xs s

setVal :: State -> Integer -> Integer -> State 
setVal ((a,b):xs) s v = if a == s then (a,v):xs else (a,b):setVal xs s v

interpret :: Ast -> [Integer] -> Integer
interpret ast args =
    let
        initState = (0,0) : zip [1..] (args ++ repeat 0)
        doneState = interpretStep ast initState
    in getVal doneState 0

varToInt = read . tail

interpretStep :: Ast -> State -> State
interpretStep (Assign to var op const) state =
    let vv = getVal state (varToInt var)
        rv = case op of
            Plus -> vv + const
            Minus -> vv - const
    in setVal state (varToInt to) rv
interpretStep (Loop var ast) state = 
    let count = getVal state (varToInt var)
        loops = foldr1 (.) $ replicate (fromIntegral count) $ interpretStep ast
    in if count == 0 then state else loops state
interpretStep a@(While var ast) state =
    let val = getVal state (varToInt var)
    in if val == 0 then state else interpretStep a $ interpretStep ast state
interpretStep (Sequence l r) state = interpretStep r $ interpretStep l state