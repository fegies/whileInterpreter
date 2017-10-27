module Turing.Parser(parseProgram) where

import Turing.TuringAst
import Turing.TuringDef

import Text.Parsec
import Text.Parsec.Char
import qualified Text.Parsec.Token as P

parseProgram :: String -> String -> Either ParseError TuringMachine
parseProgram filename content = parse parser filename content

parser = whiteSpace >> reserved "digraph" >> identifier >> braces turingParser <* eof

turingParser = do
    (states,finalStates) <- statesParser
    startState <- startStateParser
    transitions <- transitionsParser
    return $ TuringMachine states (getInputAlph transitions) (getBandAlph transitions) transitions startState finalStates

getInputAlph = const $ map fromChar ['0','1']
getBandAlph = (EmptyChar:).getInputAlph

statesParser = do
    lexeme (string "\"\"") >> brackets (reserved "shape" >> reservedOp "=" >> reserved "none") >> semi
    states <- sepEndBy (do
        name <- identifier
        brackets $ do
            reserved "shape"
            reservedOp "="
            isFinal <- (reserved "doublecircle" >> return True) <|> (reserved "circle" >> return False)
            return (isFinal,name)
        ) semi
    return (map snd states,map snd $ filter fst states)

transitionsParser = sepEndBy (do
    initialState <- identifier
    reservedOp "->"
    targetState <- identifier
    brackets $ do
        reserved "label"
        reservedOp "="
        lexeme $ char '"'
        seenChar <- fromChar <$> lexeme anyChar
        comma
        writtenChar <- fromChar <$> lexeme anyChar
        comma
        direction <- fromChar <$> lexeme anyChar
        lexeme $ char '"'
        return (initialState, seenChar, targetState, writtenChar, direction)
    ) semi

startStateParser = lexeme (string "\"\"") >> reservedOp "->" >> identifier <* semi

lexer = P.makeTokenParser turingDef
integer = P.integer lexer
reserved = P.reserved lexer
reservedOp = P.reservedOp lexer
semiSep1 = P.semiSep1 lexer
whiteSpace = P.whiteSpace lexer
braces = P.braces lexer
lexeme = P.lexeme lexer
identifier = P.identifier lexer
semi = P.semi lexer
brackets = P.brackets lexer
comma = P.comma lexer