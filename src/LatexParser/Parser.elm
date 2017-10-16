module LatexParser.Parser exposing (..)

{-| -}

import Parser exposing (..)
import LatexParser.Latex
    exposing
        ( Macro_
        , Environment_
        , InlineMath_
        , DisplayMath_
        , texComment
        , macro
        , environment
        , inlineMath
        , displayMath
        , displayMath2
        , words
        , ws
        )


type Latex
    = Macro Macro_
    | Environment Environment_
    | InlineMath InlineMath_
    | DisplayMath DisplayMath_
    | Words String
    | Comment ()


defaultLatex : Latex
defaultLatex =
    Macro (Macro_ "Parse error" [])


defaultLatexList =
    LatexList [ defaultLatex ]


latexGet r =
    r |> Result.withDefault defaultLatex


latexListGet : Result.Result Parser.Error LatexList -> List Latex
latexListGet r =
    r |> Result.withDefault defaultLatexList |> .value


{-| TEST of latex:

      > str = "\\image{http://psurl.s3.amazonaws.com/images/jc/snell2-5b65.jpg}{Refraction}{width: 250, float: right}"
      "\\image{http://psurl.s3.amazonaws.com/images/jc/snell2-5b65.jpg}{Refraction}{width: 250, float: right}"
          : String
      > import LatexParser.Parser as LP
      > import Parser as P
      > P.run LP.latex str
      Ok (Macro { name = "image", args = ["http://psurl.s3.amazonaws.com/images/jc/snell2-5b65.jpg","Refraction","width: 250, float: right"] })
          : Result.Result Parser.Error LatexParser.Parser.Latex

-}
latex : Parser Latex
latex =
    inContext "latex" <|
        oneOf
            [ map Comment texComment
            , map Environment environment
            , map DisplayMath displayMath2
            , map DisplayMath displayMath
            , map InlineMath inlineMath
            , map Macro macro
            , map Words words
            ]


type alias LatexList =
    { value : List Latex
    }


latexList : Parser LatexList
latexList =
    inContext "latexList" <|
        succeed LatexList
            |= repeat zeroOrMore latex


latexList1 : Parser (List Latex)
latexList1 =
    inContext "latexList" <|
        succeed identity
            |= repeat zeroOrMore latex



-- |. repeat zeroOrMore (oneOf [ symbol "\n", symbol " " ])
-- |. oneOf [ symbol "\n" ]
{-

    Examples

    NOTE: A terminal newline is required.

    WORDS:
    > Parser.run latexList "This is a test.\n"
    Ok { value = [Word "This",Word "is",Word "a",Word "test."] }
        : Result.Result Parser.Error LatexParser.Parser.LatexList

    NOTE: The parser stops after the first line.  BAD!
    > Parser.run latexList "This is a test.\n\nAnd so is this.\n"
    Ok { value = [Word "This",Word "is",Word "a",Word "test."] }
        : Result.Result Parser.Error LatexParser.Parser.LatexList

    INLNE MATH $ ... $ with a comment.
    > Parser.run latexList "This is it: $a^2 + b^2 = c^2$ % This is a comment.\n"
    Ok { value = [Word "This",Word "is",Word "it:",InlineMath { value = "a^2 + b^2 = c^2" },Comment ()] }
     : Result.Result Parser.Error LatexParser.Parser.LatexList

    ENVIRONMENTS
    > Parser.run latexList "\\begin{foo} yo yo \\end{foo}\n"
    Ok { value = [Environment { env = "foo", body = " yo yo " }] }
        : Result.Result Parser.Error LatexParser.Parser.LatexList

    MACRO
    Parser.run latexList "This is \\foo{test}\n"
    Ok { value = [Word "This",Word "is",Macro { name = "foo", args = ["test"] }] }
        : Result.Result Parser.Error LatexParser.Parser.LatexList

    NOTE: Error because of "." after \\foo{test}.
    Parser.run latexList "This is \\foo{test}.\n"
    Err { row = 1, col = 19, source = "This is \\foo{test}.\n", problem = BadOneOf [BadRepeat,ExpectingEnd], context = [{ row = 1, col = 9, description = "macro" },{ row = 1, col = 9, description = "latex" },{ row = 1, col = 1, description = "latexList" }] }
        : Result.Result Parser.Error LatexParser.Parser.LatexList

    WORDS, MACRO AND DISPLAYMATH:
    > Parser.run latexList "This is \\foo{test}\n\\[a^2 = 1\\]\n"
    Ok { value = [Word "This",Word "is",Macro { name = "foo", args = ["test"] },DisplayMath { value = "a^2 = 1" }] }
        : Result.Result Parser.Error LatexParser.Parser.LatexList

   NOTE: I added `|. oneOf [ symbol "\n" ]` to `latexList : Parser LatexList`.
   With this change, multiple lines are parsed.  However, the input text must end
   with TWO newlines.

   > Parser.run latexList "This is it: $a^2 + b^2 = c^2$ % This is a comment.\nho ho ho: $a^{p-1} \\equiv 1$\n\\[x^p = y^q\\]\n\n"
   Ok { value = [Word "This",Word "is",Word "it:",InlineMath { value = "a^2 + b^2 = c^2" },Comment (),Word "ho",Word "ho",Word "ho:",InlineMath { value = "a^{p-1} \\equiv 1" },DisplayMath { value = "x^p = y^q" }] }
     : Result.Result Parser.Error LatexParser.Parser.LatexList
-}
