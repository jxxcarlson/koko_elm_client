module LatexParser.Parser exposing (..)

import Parser exposing (..)
import LatexParser.Latex
    exposing
        ( Macro_
        , Environment_
        , InlineMath_
        , DisplayMath_
        , Words_
        , macro
        , environment
        , inlineMath
        , displayMath
        , words
        )


type Latex
    = Macro Macro_
    | Environment Environment_
    | InlineMath InlineMath_
    | DisplayMath DisplayMath_
    | Words Words_


latex : Parser Latex
latex =
    inContext "latex" <|
        oneOf
            [ map Environment environment
            , map Macro macro
            , map DisplayMath displayMath
            , map InlineMath inlineMath
            , map Words words
            ]


type alias LatexList =
    { value : List Latex
    }


latexList : Parser LatexList
latexList =
    inContext "words" <|
        succeed LatexList
            |= repeat zeroOrMore latex
