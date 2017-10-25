module Main(main) where

import Parser(parseProgram)
import Interpreter(interpret)
import Text.Show.Pretty

main = do
    cont <- readFile "test.while"
    let ast = parseProgram "test.while" cont
    let s = ppShow ast
    putStrLn s
    case ast of
        Left err -> fail $ show err
        Right ast -> do
            let res = interpret ast [1]
            print res
    