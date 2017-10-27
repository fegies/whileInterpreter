module While.Parser(parseProgram) where

import While.WhileAst
import While.WhileDef
import Text.Parsec
import Text.Parsec.Char

import qualified Text.Parsec.Token as P

parseProgram :: String -> String -> Either ParseError Ast
parseProgram filename content = parse parser filename content

parser = whiteSpace >> sequenceParser <* eof

astParser = loopParser <|> whileParser <|> assignParser

variableParser = lexeme $ do
    char 'x'
    i <- many1 digit
    return $ 'x' : i

loopParser = Loop
    <$ reserved "LOOP"
    <*> variableParser
    <* reserved "DO"
    <*> sequenceParser
    <* reserved "DONE"

constantParser = integer

operatorParser =
    (reservedOp "+" >> return Plus) <|>
    (reservedOp "-" >> return Minus)

assignParser = Assign
    <$> variableParser
    <* reservedOp ":="
    <*> variableParser
    <*> operatorParser
    <*> constantParser

whileParser = While
    <$ reserved "WHILE"
    <*> variableParser
    <* reserved "DO"
    <*> sequenceParser
    <* reserved "DONE"

sequenceParser = fmap (foldr1 Sequence) (semiSep1 astParser)

lexer = P.makeTokenParser whileDef
integer = P.integer lexer
reserved = P.reserved lexer
reservedOp = P.reservedOp lexer
semiSep1 = P.semiSep1 lexer
whiteSpace = P.whiteSpace lexer
lexeme = P.lexeme lexer