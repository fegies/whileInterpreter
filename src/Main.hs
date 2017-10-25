module Main(main) where

import Parser(parseProgram)
import Text.Show.Pretty

main = do
    cont <- readFile "test.while"
    let s = ppShow $ parseProgram "test.while" cont
    putStrLn s