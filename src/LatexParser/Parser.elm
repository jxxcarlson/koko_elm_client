module LatexParser.Parser exposing (..)

{-| -}

import Parser exposing (..)
import LatexParser.ParserTypes exposing (InlineMath_, DisplayMath_, Macro_, Environment_, Latex(..))
import LatexParser.Latex
    exposing
        ( texComment
        , macro
        , environment
        , inlineMath
        , displayMath
        , displayMath2
        , words
        , ws
        )


defaultLatexList =
    [ Macro (Macro_ "Parse error" []) ]


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


latexParser : Parser (List Latex)
latexParser =
    inContext "latexParser" <|
        succeed identity
            |= repeat zeroOrMore latex



-- |. repeat zeroOrMore (oneOf [ symbol "\n", symbol " " ])
-- |. oneOf [ symbol "\n" ]
{-

    Examples

    NOTE: A terminal newline is required.

    WORDS:
    > Parser.run latexParser "This is a test.\n"
    Ok { value = [Word "This",Word "is",Word "a",Word "test."] }
        : Result.Result Parser.Error LatexParser.Parser.LatexList

    NOTE: The parser stops after the first line.  BAD!
    > Parser.run latexParser "This is a test.\n\nAnd so is this.\n"
    Ok { value = [Word "This",Word "is",Word "a",Word "test."] }
        : Result.Result Parser.Error LatexParser.Parser.LatexList

    INLNE MATH $ ... $ with a comment.
    > Parser.run latexParser "This is it: $a^2 + b^2 = c^2$ % This is a comment.\n"
    Ok { value = [Word "This",Word "is",Word "it:",InlineMath { value = "a^2 + b^2 = c^2" },Comment ()] }
     : Result.Result Parser.Error LatexParser.Parser.LatexList

    ENVIRONMENTS
    > Parser.run latexParser "\\begin{foo} yo yo \\end{foo}\n"
    Ok { value = [Environment { env = "foo", body = " yo yo " }] }
        : Result.Result Parser.Error LatexParser.Parser.LatexList

    MACRO
    Parser.run latexParser "This is \\foo{test}\n"
    Ok { value = [Word "This",Word "is",Macro { name = "foo", args = ["test"] }] }
        : Result.Result Parser.Error LatexParser.Parser.LatexList

    NOTE: Error because of "." after \\foo{test}.
    Parser.run latexParser "This is \\foo{test}.\n"
    Err { row = 1, col = 19, source = "This is \\foo{test}.\n", problem = BadOneOf [BadRepeat,ExpectingEnd], context = [{ row = 1, col = 9, description = "macro" },{ row = 1, col = 9, description = "latex" },{ row = 1, col = 1, description = "latexParser" }] }
        : Result.Result Parser.Error LatexParser.Parser.LatexList

    WORDS, MACRO AND DISPLAYMATH:
    > Parser.run latexParser "This is \\foo{test}\n\\[a^2 = 1\\]\n"
    Ok { value = [Word "This",Word "is",Macro { name = "foo", args = ["test"] },DisplayMath { value = "a^2 = 1" }] }
        : Result.Result Parser.Error LatexParser.Parser.LatexList

   NOTE: I added `|. oneOf [ symbol "\n" ]` to `latexParser : Parser LatexList`.
   With this change, multiple lines are parsed.  However, the input text must end
   with TWO newlines.

   > Parser.run latexParser "This is it: $a^2 + b^2 = c^2$ % This is a comment.\nho ho ho: $a^{p-1} \\equiv 1$\n\\[x^p = y^q\\]\n\n"
   Ok { value = [Word "This",Word "is",Word "it:",InlineMath { value = "a^2 + b^2 = c^2" },Comment (),Word "ho",Word "ho",Word "ho:",InlineMath { value = "a^{p-1} \\equiv 1" },DisplayMath { value = "x^p = y^q" }] }
     : Result.Result Parser.Error LatexParser.Parser.LatexList
-}
