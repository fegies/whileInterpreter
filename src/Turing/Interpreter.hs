module Turing.Interpreter(interpret) where

import Turing.TuringAst
import Data.List

type Configuration = ([TuringChar],TuringState,[TuringChar])

left :: Configuration -> [TuringChar]
left (l,_,_) = l
state :: Configuration -> TuringState
state (_,s,_) = s
right :: Configuration -> [TuringChar]
right (_,_,r) = r

toBinary :: Integer -> [TuringChar]
toBinary = reverse . map fromChar . toBinary' where
    toBinary' a
        | a == 0 = ['0']
        | a == 1 = ['1']
        | otherwise = toBinary' (a `mod` 2) ++ toBinary' (a `div` 2)

fromBinary :: [TuringChar] -> Integer
fromBinary = foldr fromBinary' 0 . reverse . dropWhile isNull where
    fromBinary' (NonemptyChar '0') acc = 2 * acc
    fromBinary' (NonemptyChar '1') acc = 2 * acc + 1
    isNull (NonemptyChar '0') = True
    isNull _ = False

isNonempty :: TuringChar -> Bool
isNonempty EmptyChar = False
isNonempty _ = True

interpret :: TuringMachine -> [Integer] -> IO Integer
interpret tm input =
    let initialState = (repeat EmptyChar,startState tm, intercalate [EmptyChar] (map toBinary input) ++ repeat EmptyChar)
    in fromBinary . takeWhile isNonempty. right <$> runTuring tm initialState

runTuring :: TuringMachine -> Configuration -> IO Configuration
runTuring tm conf =
    let s = state conf
        c = head $ right conf
        curins = find (\(is,ic,_,_,_)-> is == state conf && ic == c) $ transitions tm
        execInstruction (_,_,ns,wc,dir) (l,_,_:r) = execMove dir (l,ns,wc:r)
        execMove DirectionN conf = conf
        execMove DirectionR (l,s,r:rs) = (r:l,s,rs)
        execMove DirectionL (l:ls,s,r) = (ls,s,l:r)
    in if s `elem` finalStates tm then return conf else case curins of
        Nothing -> return conf
        Just ins -> do
            print ins
            runTuring tm $ execInstruction ins conf
