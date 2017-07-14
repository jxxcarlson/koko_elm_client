module LatexParser.Parser exposing (..)

{-|


-}

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
        , word
        )


type Latex
    = Macro Macro_
    | Environment Environment_
    | InlineMath InlineMath_
    | DisplayMath DisplayMath_
    | Words Words_
    | Word String


defaultLatex : Latex
defaultLatex =
    Macro (Macro_ "uu" [])


defaultLatexList =
    LatexList [ defaultLatex ]


latexGet r =
    r |> Result.withDefault defaultLatex


latexListGet : Result.Result Parser.Error LatexList -> List Latex
latexListGet r =
    r |> Result.withDefault defaultLatexList |> .value


latex : Parser Latex
latex =
    inContext "latex" <|
        oneOf
            [ map Environment environment
            , map Macro macro
            , map DisplayMath displayMath
            , map InlineMath inlineMath
            , map Word word
            ]


type alias LatexList =
    { value : List Latex
    }


latexList : Parser LatexList
latexList =
    inContext "latexList" <|
        succeed LatexList
            |= repeat zeroOrMore latex
