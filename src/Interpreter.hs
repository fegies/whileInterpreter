module Interpreter(interpret) where

import WhileAst
import Data.Map.Lazy as Map

interpret :: Ast -> [Integer] -> Integer
interpret ast args =
    let
        initList = (0,0) : zip [1..] (args ++ repeat 0)
        initialVars = Map.fromList initList
        doneState = interpretStep ast initialVars
    in done

interpretStep :: Ast -> Map.Map Integer Integer -> Map.Map Integer Integer
interpretStep (Assign to var op const) state =
    Map.insert 