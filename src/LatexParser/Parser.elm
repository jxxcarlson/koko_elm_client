module LatexParser.Parser exposing (..)

import Parser exposing (..)
import LatexParser.Latex
    exposing
        ( Macro1
        , Macro2
        , Environment
        , macro1
        , macro2
        , environment
        )


type Latex
    = M1 Macro1
    | M2 Macro2
    | Env Environment


latex : Parser Latex
latex =
    oneOf
        [ map M1 macro1
        , map M2 macro2
        , map Env environment
        ]


environment : Parser Latex
environment =
    succeed Latex
        |. symbol "\\"
        |. (keyword "begin")
        |. symbol "{"
        |= keep zeroOrMore (\c -> c /= '}')
        |. symbol "}"
        |= keep zeroOrMore (\c -> c /= '\\')
        |. symbol "\\"
        |. (keyword "end")
        |. symbol "{"
        |. keep zeroOrMore (\c -> c /= '}')
        |. symbol "}"
        |. ignore (Exactly 1) (\c -> c == ' ')


macro1 : Parser Latex
macro1 =
    succeed Latex
        |. symbol "\\"
        |= keep zeroOrMore (\c -> c /= '{')
        |. symbol "{"
        |= keep zeroOrMore (\c -> c /= '}')
        |. symbol "}"
        |. ignore (Exactly 1) (\c -> c == ' ')


macro2 : Parser Latex
macro2 =
    succeed Latex
        |. symbol "\\"
        |= keep zeroOrMore (\c -> c /= '{')
        |. symbol "{"
        |= keep zeroOrMore (\c -> c /= '}')
        |. symbol "}"
        |. symbol "{"
        |= keep zeroOrMore (\c -> c /= '}')
        |. symbol "}"
        |. ignore (Exactly 1) (\c -> c == ' ')
