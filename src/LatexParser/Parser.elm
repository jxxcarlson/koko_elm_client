module LatexParser.Parser exposing (..)

import Parser exposing (..)
import LatexParser.Latex
    exposing
        ( MA
        , ARG
        , Env
        , macro
        , environment
        )


type Latex
    = Macro MA
    | Environment Env


latex : Parser Latex
latex =
    oneOf
        [ map Environment environment
        , map Macro macro
        ]
