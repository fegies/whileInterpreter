module Main(main) where

import qualified While.Parser as W
import qualified While.Interpreter as W
import qualified Turing.Parser as T
import qualified Turing.Interpreter as T
import System.Environment

main = getArgs >>= parseArgs

parseArgs (mode:file:params) = do
    cont <- readFile file
    let progargs = map read params
    case mode of
        "while" -> case W.parseProgram file cont of
            Left err -> print err
            Right ast -> print $ W.interpret ast progargs
        "turing" -> case T.parseProgram file cont of
            Left err -> print err
            Right ast -> do
                print ast
                T.interpret ast progargs >>= print
parseArgs _ = putStrLn "Usage: whileInterpreter MODE Filename [LIST OF INTEGER ARGUMENTS FOR PROGRAM]"
