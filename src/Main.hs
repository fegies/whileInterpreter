module Main(main) where

import Parser(parseProgram)
import Interpreter(interpret)
import System.Environment

main = getArgs >>= parseArgs

parseArgs [] = putStrLn "Usage: whileInterpreter FILENAME [LIST OF INTEGER ARGUMENTS FOR PROGRAM]"
parseArgs (file:params) = do
    cont <- readFile file
    let progargs = map read params
    case parseProgram file cont of
        Left err -> print err
        Right ast -> print $ interpret ast progargs
