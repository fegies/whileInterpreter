module While.WhileDef where

import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.Language

whileDef :: LanguageDef st
whileDef = emptyDef
            {
                identStart = lower,
                identLetter = letter <|> char '_' <|> digit,
                reservedOpNames = ["+", "-", ":=", "*"],
                reservedNames = ["WHILE", "LOOP", "DO", "END"]
            }