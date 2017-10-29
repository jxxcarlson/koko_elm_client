module MiniLatex.Parser exposing (..)

import Parser exposing (..)


{- ELLIE: https://ellie-app.com/pcB5b3BPfa1/0

   https://ellie-app.com/pcB5b3BPfa1/1

-}


{-| Types
-}
type LatexExpression
    = LXString String
    | Comment String
    | Item Int LatexExpression
    | InlineMath String
    | DisplayMath String
    | Macro String (List String)
    | Environment String LatexExpression
    | LatexList (List LatexExpression)



{- PARSER: TOP LEVEL -}


latexList : Parser LatexExpression
latexList =
    succeed identity
        |= repeat oneOrMore parse
        |> map LatexList


parse : Parser LatexExpression
parse =
    oneOf
        [ texComment
        , lazy (\_ -> environment)
        , displayMathDollar
        , displayMathBrackets
        , inlineMath
        , macro
        , words
        ]



{- PARSER HELPERS -}


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



{- PARSE WORDS -}


word : Parser String
word =
    inContext "word" <|
        succeed identity
            |. spaces
            |= keep oneOrMore (\c -> not (c == ' ' || c == '\n' || c == '\\' || c == '$'))
            |. ignore zeroOrMore (\c -> c == ' ' || c == '\n')


{-| Like `word`, but after a word is recognized spaces, not spaces + newlines are consumed
-}
word2 : Parser String
word2 =
    inContext "word" <|
        succeed identity
            |. spaces
            |= keep oneOrMore (\c -> not (c == ' ' || c == '\n' || c == '\\' || c == '$'))
            |. spaces


words : Parser LatexExpression
words =
    inContext "words" <|
        (succeed identity
            |= repeat oneOrMore word
            |> map (String.join " ")
            |> map LXString
        )


{-| Like `words`, but after a word is recognized spaces, not spaces + newlines are consumed
-}
words2 : Parser LatexExpression
words2 =
    inContext "words2" <|
        (succeed identity
            |= repeat oneOrMore word2
            |> map (String.join " ")
            |> map LXString
        )


texComment : Parser LatexExpression
texComment =
    symbol "%"
        |. ignoreUntil "\n"
        |> source
        |> map Comment



{- ITEMIZED LISTS -}


item : Parser LatexExpression
item =
    succeed identity
        |. ws
        |. keyword "\\item"
        |. spaces
        |= repeat zeroOrMore (oneOf [ words2, inlineMath2, macro2 ])
        |. symbol "\n"
        |. spaces
        |> map (\x -> Item 1 (LatexList x))


itemitem : Parser LatexExpression
itemitem =
    succeed identity
        |. ws
        |. keyword "\\itemitem"
        |. spaces
        |= repeat zeroOrMore (oneOf [ words2, inlineMath2, macro2 ])
        |. symbol "\n"
        |. spaces
        |> map (\x -> Item 2 (LatexList x))



{- MATHEMATICAL TEXT -}


inlineMath : Parser LatexExpression
inlineMath =
    inContext "inline math" <|
        succeed InlineMath
            |. symbol "$"
            |= parseUntil "$"
            |. ws


{-| Like `inlineMath`, but only spaces, note spaces + newlines, are consumed after recognizing an element
-}
inlineMath2 : Parser LatexExpression
inlineMath2 =
    inContext "inline math" <|
        succeed InlineMath
            |. symbol "$"
            |= parseUntil "$"
            |. spaces


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



{- MACROS -}
{- NOTE: macro sequences should be of the form "" followed by alphabetic characterS,
   but not equal to certain reserved words, e.g., "\begin", "\end", "\item"
-}


macro : Parser LatexExpression
macro =
    inContext "macro" <|
        succeed Macro
            |= macroName
            |= repeat zeroOrMore arg
            |. ws


{-| Like macro, but only spaces, not spaces + nelines are consumed after recognition
-}
macro2 : Parser LatexExpression
macro2 =
    inContext "macro" <|
        succeed Macro
            |= macroName
            |= repeat zeroOrMore arg
            |. spaces


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
                case res of
                    Err e ->
                        fail e

                    Ok _ ->
                        succeed ()
            )


reservedWord : Parser ()
reservedWord =
    succeed identity
        |= oneOf [ symbol "\\begin", keyword "\\end", keyword "\\item" ]



{- ENVIRONMENTS -}


environment : Parser LatexExpression
environment =
    lazy (\_ -> beginWord |> andThen environmentOfType)


environmentOfType : String -> Parser LatexExpression
environmentOfType envType =
    let
        endWord =
            "\\end{" ++ envType ++ "}"
    in
        case envType of
            "enumerate" ->
                itemEnvironmentBody endWord envType

            "itemize" ->
                itemEnvironmentBody endWord envType

            _ ->
                standardEnvironmentBody endWord envType


standardEnvironmentBody endWord envType =
    succeed identity
        |. ws
        |= repeat zeroOrMore parse
        |. ws
        |. symbol endWord
        |. ws
        |> map LatexList
        |> map (Environment envType)


itemEnvironmentBody endWord envType =
    succeed identity
        |. ws
        |= repeat zeroOrMore (oneOf [ itemitem, item ])
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
