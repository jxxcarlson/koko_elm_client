module LatexParser.Parser exposing (..)

{-|

    VALID INPUTS:

    LATEX:

    r = run latex "a b c \n"
    Ok (Words { value = ["a","b","c"] })
      : Result.Result Parser.Error LatexParser.Parser.Latex

    r = run latex "a b c\n"
    Ok (Words { value = ["a","b","c"] })
      : Result.Result Parser.Error LatexParser.Parser.Latex

    r = run latex "\\foo{bar} "
    Ok (Macro { name = "foo", args = ["bar"] })
      : Result.Result Parser.Error LatexParser.Parser.Latex

    r = run latex "\\foo{bar}\n"
    Ok (Macro { name = "foo", args = ["bar"] })
      : Result.Result Parser.Error LatexParser.Parser.Latex


    LATEXLIST:

    r = run latexList "a b c \n" OR "a b c\n"
    Ok { value = [Words { value = ["a","b","c"] }] }
      : Result.Result Parser.Error LatexParser.Parser.LatexList

    r = run latexList "a b c\nx y z\n"
    Ok { value = [Words { value = ["a","b","c"] },Words { value = ["x","y","z"] }] }
      : Result.Result Parser.Error LatexParser.Parser.LatexList


    INVALID INPUTS:

    r = run latex "a b c"
    Err { row = 1, col = 6, source = "a b c", problem = ExpectingSymbol "\n", context = [{ row = 1, col = 1, description = "words" },{ row = 1, col = 1, description = "latex" }] }
      : Result.Result Parser.Error LatexParser.Parser.Latex

    r = run latexList "a b c \\foo{bar}\nx y z\n"
    Err { row = 1, col = 7, source = "a b c \\foo{bar}\nx y z\n", problem = ExpectingSymbol "\n", context = [{ row = 1, col = 1, description = "words" },{ row = 1, col = 1, description = "latex" },{ row = 1, col = 1, description = "words" }] }
      : Result.Result Parser.Error LatexParser.Parser.LatexList
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


latexListGet r =
    r |> Result.withDefault defaultLatexList


{-|
  See also (per @jessta): Result.map

  Sample test:
-}
rr =
    run latexList "a b c\n d e f\n"


vv =
    .value (latexListGet rr)


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
    inContext "words" <|
        succeed LatexList
            |= repeat zeroOrMore latex
