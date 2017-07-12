module LatexParser.Parser exposing (..)

import Parser exposing (..)
import LatexParser.Latex
    exposing
        ( M1
        , M2
        , Env
        , macro1
        , macro2
        , environment
        )


type Latex
    = Macro1 M1
    | Environment Env


latex : Parser Latex
latex =
    oneOf
        [ map Environment environment
        , map Macro1 macro1
        ]
