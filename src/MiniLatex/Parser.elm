module MiniLatex.Parser exposing (..)

import Parser exposing (..)


{-| Types
-}
type LatexExpression
    = LXString String
    | InlineMath String
    | DisplayMath String
    | Macro String (List String)
    | Environment String LatexExpression
    | Nested (List LatexExpression)


{-| Parser top level
-}
parseElement : Parser LatexExpression
parseElement =
    inContext "parseElement" <|
        oneOf
            [ environment
            , displayMathDollar
            , displayMathBrackets
            , inlineMath
            , macro
            , words
            ]


parseListHelper : Parser (List LatexExpression)
parseListHelper =
    inContext "parseList" <|
        succeed identity
            |= repeat zeroOrMore parseElement


parseList : Parser LatexExpression
parseList =
    parseListHelper |> map Nested


{-| Parser helpers
-}
spaces : Parser ()
spaces =
    ignore zeroOrMore (\c -> c == ' ')


ws : Parser ()
ws =
    ignore zeroOrMore (\c -> c == ' ' || c == '\n')


parseUntil : String -> Parser String
parseUntil marker =
    ignoreUntil marker
        |> source
        |> map (String.dropRight <| String.length marker)


arg : Parser String
arg =
    succeed identity
        |. symbol "{"
        |= parseUntil "}"


{-| Parsers
-}
word : Parser String
word =
    inContext "word" <|
        succeed identity
            |. spaces
            |= keep oneOrMore (\c -> not (c == ' ' || c == '\n' || c == '\\' || c == '$'))
            |. ignore zeroOrMore (\c -> c == ' ' || c == '\n')


words : Parser LatexExpression
words =
    inContext "words" <|
        (succeed identity
            |= repeat oneOrMore word
            |> map (String.join " ")
            |> map LXString
        )


inlineMath : Parser LatexExpression
inlineMath =
    inContext "inline math" <|
        succeed InlineMath
            |. symbol "$"
            |= parseUntil "$"
            |. ws


displayMathDollar : Parser LatexExpression
displayMathDollar =
    inContext "display math" <|
        succeed DisplayMath
            |. ignore zeroOrMore ((==) ' ')
            |. symbol "$$"
            |= parseUntil "$$"


displayMathBrackets : Parser LatexExpression
displayMathBrackets =
    inContext "display math" <|
        succeed DisplayMath
            |. ignore zeroOrMore ((==) ' ')
            |. symbol "\\["
            |= parseUntil "\\]"


macro : Parser LatexExpression
macro =
    inContext "macro" <|
        succeed Macro
            |. spaces
            |. symbol "\\"
            |= keep zeroOrMore (\c -> c /= '{')
            |= repeat zeroOrMore arg
            |. ws


environment : Parser LatexExpression
environment =
    inContext "environment"
        (beginWord |> andThen environmentOfType)


environmentOfType : String -> Parser LatexExpression
environmentOfType envType =
    let
        endWord =
            "\\end{" ++ envType ++ "}"
    in
        ignoreUntil endWord
            |> source
            |> map (String.dropRight (String.length endWord))
            |> map LXString
            |> map (Environment envType)


beginWord : Parser String
beginWord =
    succeed identity
        |. ignore zeroOrMore ((==) ' ')
        |. symbol "\\begin{"
        |= parseUntil "}"


{-| Examples
-}
lxstring =
    LXString "foo"


lxstring2 =
    run parseElement "This is a test."


inlineMathExpr =
    InlineMath "a^2 = 7"


inlineMathExpr2 =
    run parseElement "$a^2 = 7$"


displayMathExpr =
    InlineMath "b^2 = 3"


displayMathExpr2 =
    run parseElement "$$b^2 = 3$$"


displayMathExpr3 =
    run parseElement "\\[b^2 = 3\\]"


macroExpr =
    Macro "image" [ "http://foo.io/bird.jpg", "Bird" ]


macroExpr2 =
    run parseElement "\\image{http://foo.io/bird.jpg}{Bird}"


envExpr =
    Environment "theorem" (LXString "Infinity is very large")


envExpr2 =
    run parseElement "\\begin{theorem}\nInfinity is very large\\end{theorem}"


env2 =
    Environment "theorem" <|
        Nested
            [ LXString "An equation of the form"
            , InlineMath "a^n = 1"
            , LXString "has"
            , InlineMath "n"
            , LXString "distinct complex solutions"
            ]


ll =
    [ lxstring, envExpr ]


ll2 =
    run parseList "This is a test: $a^2 = 7$"
