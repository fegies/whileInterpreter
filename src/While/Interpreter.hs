{-# LANGUAGE BangPatterns #-}
module While.Interpreter(interpret) where

import While.WhileAst
import qualified Data.Map.Strict as Map
import Data.Maybe(fromMaybe)

type State = Map.Map Integer Integer

getVal :: State -> Integer -> Integer
getVal state var = fromMaybe 0 $ Map.lookup var state

setVal :: State -> Integer -> Integer -> State 
setVal state var val = Map.insert var val state

interpret :: Ast -> [Integer] -> IO Integer
interpret ast args = do
    let initState = Map.fromList $ (0,0) : zip [1..] args
    doneState <- interpretStep ast initState
    return $ getVal doneState 0

varToInt = read . tail

interpretStep :: Ast -> State -> IO State
interpretStep (Assign to var op const) !state =
    let vv = getVal state (varToInt var)
        rv = case op of
            Plus -> vv + const
            Minus -> if vv < const then 0 else vv - const
            Mult -> vv * const
    in return $ setVal state (varToInt to) rv
interpretStep a@(Loop var ast) !state = 
    let count = getVal state (varToInt var)
        loops = replicate (fromIntegral count) $ interpretStep ast
        runloops count state = if count == 0 then return state else interpretStep ast state >>= runloops (count-1)
    in do
        putStrLn $ show a ++ " ; count=" ++ show count
        runloops count state
interpretStep a@(While var ast) !state =
    let val = getVal state (varToInt var)
    in if val == 0 then return state else interpretStep ast state >>= interpretStep a
interpretStep (Sequence l r) !state = interpretStep l state >>= interpretStep r