module MiniLatex.Parser exposing (..)

import Parser exposing (..)


{-| Types
-}
type LatexExpression
    = LXString String
    | Comment String
    | InlineMath String
    | DisplayMath String
    | Macro String (List String)
    | Environment String LatexExpression
    | LatexList (List LatexExpression)


{-| Parser top level
-}
parse : Parser LatexExpression
parse =
    oneOf
        [ texComment
        , environment
        , displayMathDollar
        , displayMathBrackets
        , inlineMath
        , macro
        , words
        ]


latexList : Parser LatexExpression
latexList =
    succeed identity
        |= repeat oneOrMore parse
        |> map LatexList


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


texComment : Parser LatexExpression
texComment =
    symbol "%"
        |. ignoreUntil "\n"
        |> source
        |> map Comment



-- |. ignore zeroOrMore (\c -> c == ' ' || c == '\n')


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
