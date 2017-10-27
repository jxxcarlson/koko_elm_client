module MiniLatex.Parser exposing (..)

import Parser exposing (..)


{- ELLIE: https://ellie-app.com/pcB5b3BPfa1/0

   https://ellie-app.com/pcB5b3BPfa1/1

-}
{- From Ilias

   there is, but it ain't pretty...
   ```mustFail : Parser a -> Parser ()
   mustFail parser =
     oneOf
       [ delayedCommitMap always parser (succeed ()) |> map (always <| Err "I didn't fail")
       , succeed (Ok ())
       ]
       |> andThen (\res ->
         case res of
           Err e -> fail e
           Ok _ -> succeed ()
       )
   ```


   [5:49]
   (didn't type that in an editor so no idea if it's syntactically correct, but should be close enough)


   [5:50]
   using that, something like `succeed identity |. mustFail reservedWork |. macroName`.
   To make _that_ entire thing backtrack on failure,
   you can wrap that with another `delayedCommitMap` thing.
    Beware, though, backtracking is expensive :

-}


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


{-| BEGIN PARSER DEFINITINS
-}
macroName : Parser String
macroName =
    allOrNothing <|
        succeed identity
            |. mustFail reservedWord
            |= innerMacroName


innerMacroName : Parser String
innerMacroName =
    inContext "macroName" <|
        succeed identity
            |. spaces
            |. symbol "\\"
            |= keep zeroOrMore (\c -> not (c == '{' || c == ' '))


allOrNothing : Parser a -> Parser a
allOrNothing parser =
    delayedCommitMap always parser (succeed ())


mustFail : Parser a -> Parser ()
mustFail parser =
    oneOf
        [ delayedCommitMap always parser (succeed ()) |> map (always <| Err "I didn't fail")
        , succeed (Ok ())
        ]
        |> andThen
            (\res ->
                case Debug.log "res" res of
                    Err e ->
                        fail e

                    Ok _ ->
                        succeed ()
            )


reservedWord : Parser ()
reservedWord =
    succeed identity
        |= oneOf [ symbol "\\begin", keyword "\\end", keyword "\\item" ]


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


innerParse : Parser LatexExpression
innerParse =
    oneOf
        [ texComment
        , displayMathDollar
        , displayMathBrackets
        , inlineMath
        , lazy (\_ -> environment)
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
            |. symbol "$$"
            |= parseUntil "$$"
            |. ws


displayMathBrackets : Parser LatexExpression
displayMathBrackets =
    inContext "display math" <|
        succeed DisplayMath
            |. ignore zeroOrMore ((==) ' ')
            |. symbol "\\["
            |= parseUntil "\\]"


endMacro : Parser LatexExpression
endMacro =
    fail "\\end{"


{-| NOTE: macro sequences should be of the form "" followed by alphabetic characterS,
but not equal to certain reserved words, e.g., "\begin", "\end", "\item"
-}
macro : Parser LatexExpression
macro =
    inContext "macro" <|
        succeed Macro
            |= macroName
            |= repeat zeroOrMore arg
            |. ws


environment : Parser LatexExpression
environment =
    lazy (\_ -> beginWord |> andThen environmentOfType)


environmentOfType : String -> Parser LatexExpression
environmentOfType envType =
    let
        endWord =
            "\\end{" ++ envType ++ "}"
    in
        succeed identity
            |. ws
            |= repeat zeroOrMore innerParse
            |. ws
            |. symbol endWord
            |. ws
            |> map LatexList
            |> map (Environment envType)


beginWord : Parser String
beginWord =
    succeed identity
        |. ignore zeroOrMore ((==) ' ')
        |. symbol "\\begin{"
        |= parseUntil "}"
