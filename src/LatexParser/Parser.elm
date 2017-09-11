module LatexParser.Parser exposing (..)

{-| -}

import Parser exposing (..)
import LatexParser.Latex
    exposing
        ( Macro_
        , Environment_
        , InlineMath_
        , DisplayMath_
        , Words_
        , texComment
        , macro
        , environment
        , inlineMath
        , displayMath
        , displayMath2
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
    | Comment ()


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
            [ map Comment texComment
            , map Environment environment
            , map DisplayMath displayMath2
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
