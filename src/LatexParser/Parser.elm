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
