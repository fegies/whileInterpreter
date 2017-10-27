module Turing.TuringDef where
    
import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.Language

turingDef :: LanguageDef st
turingDef = emptyDef
            {
                identStart = lower,
                identLetter = letter <|> char '_' <|> digit,
                reservedOpNames = ["->", "=", "="],
                reservedNames = ["digraph", "shape", "none", "circle", "doublecircle", "label"]
            }