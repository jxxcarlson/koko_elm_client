module LatexParser.Latex
    exposing
        ( Environment_
        , Macro_
        , InlineMath_
        , DisplayMath_
        , Words_
        , macro
        , environment
        , inlineMath
        , displayMath
        , displayMath2
        , endWord_
        , word
        , words
        , texComment
        , ws
        )

{-| LatexParser parses an as-yet undetermined subset
of LaTeX into elements like

"\foo{bar} " => Macro
Macro2, e.g., a "\foo{bar}{baz}"
Environment, e.g., a "\begin{ENV} BODY \end{ENV}"
InlineMath, e.g., a "$ a^2 + b^2 + c^2 $"
DisplayMpath, e.g., a "[ a^2 + b^2 + c^2 ]"

The parsed text will be used to construct a mixture
of HTML, inlne LateX, and display LaTeX that can
be rendered by a browser + MathJax.

Recall that MathJax does not process other than inlne and
display LateX. The aim, therefore, is to properly render
not only these, but constructs like \emph{foobar},

\begin{theorem}
Blah Blah
\end{theorem}

Etc.

-}

-- import Parser exposing (Parser, (|.), (|=), succeed, symbol, float, ignore, zeroOrMore)

import Char
import Parser exposing (..)
import Parser.LanguageKit exposing (variable)
import Regex
import Set


spaces : Parser ()
spaces =
    ignore zeroOrMore (\c -> c == ' ')


ws : Parser ()
ws =
    ignore zeroOrMore (\c -> c == ' ' || c == '\n')


word : Parser String
word =
    inContext "word" <|
        succeed identity
            |. spaces
            |= keep oneOrMore (\c -> not (c == ' ' || c == '\n' || c == '\\'))
            |. ignore zeroOrMore (\c -> c == ' ' || c == '\n')


type alias Words_ =
    { value : List String
    }


words : Parser Words_
words =
    inContext "words" <|
        succeed Words_
            |= repeat zeroOrMore word
            |. oneOf [ symbol "\n", symbol "\\" ]


type alias Environment_ =
    { env : String
    , body : String
    }


type alias InlineMath_ =
    { value : String
    }


inlineMath : Parser InlineMath_
inlineMath =
    inContext "inline math" <|
        succeed InlineMath_
            |. symbol "$"
            |= keep zeroOrMore (\c -> c /= '$')
            |. symbol "$"
            |. ignore zeroOrMore (\c -> c == ' ' || c == '\n')


type alias DisplayMath_ =
    { value : String
    }


displayMath : Parser DisplayMath_
displayMath =
    inContext "display math" <|
        delayedCommit (symbol "$$") <|
            succeed
                DisplayMath_
                |= keep zeroOrMore (\c -> c /= '$')
                |. symbol "$$"
                |. ignore oneOrMore (\c -> c == ' ' || c == '\n')


displayMath2 : Parser DisplayMath_
displayMath2 =
    inContext "display math" <|
        delayedCommit (symbol "\\[") <|
            succeed
                DisplayMath_
                |= keep zeroOrMore (\c -> c /= '\\')
                |. symbol "\\]"
                |. ignore (Exactly 1) (\c -> c == ' ' || c == '\n')
                |. spaces


type alias Macro_ =
    { name : String
    , args : List String
    }


arg : Parser String
arg =
    succeed identity
        |. symbol "{"
        |= keep zeroOrMore (\c -> c /= '}')
        |. symbol "}"


{-| run macro "\foo{bar} "
run macro "\foo{bar}{baz} "
-}
macro : Parser Macro_
macro =
    inContext "macro" <|
        succeed Macro_
            |. spaces
            |. symbol "\\"
            |= keep zeroOrMore (\c -> c /= '{')
            |= repeat zeroOrMore arg
            |. oneOf [ ignore (Exactly 1) (\c -> c == ' ' || c == '\n'), Parser.end ]
            -- |. spaces
            |. ignore zeroOrMore (\c -> c == ' ' || c == '\n')


texComment : Parser ()
texComment =
    symbol "%"
        |. ignoreUntil "\n"
        |. ignore zeroOrMore (\c -> c == ' ' || c == '\n')



{- BEGIN ILIAS' CODE -}


environment : Parser Environment_
environment =
    inContext "environment"
        (beginWord |> andThen environmentOfType)


environmentOfType : String -> Parser Environment_
environmentOfType envType =
    succeed (Environment_ envType)
        |= stringExceptEnd
        |. endWord_ envType


beginWord : Parser String
beginWord =
    succeed identity
        |. ignore zeroOrMore ((==) ' ')
        |. symbol "\\begin{"
        |= keep zeroOrMore (\c -> c /= '}')
        |. symbol "}"


endWord_ : String -> Parser ()
endWord_ envType =
    succeed ()
        |. ignore zeroOrMore ((==) ' ')
        |. keyword "\\end{"
        |. keyword envType
        |. symbol "}"


word_ : Parser String
word_ =
    succeed (++)
        |. ignore zeroOrMore (\c -> c == ' ')
        |= keep (Exactly 1) (\c -> c /= ' ')
        |= keep zeroOrMore (\c -> (c /= ' ') && (c /= '\\'))


wordExceptEnd : Parser String
wordExceptEnd =
    delayedCommitMap always
        (word_
            |> andThen
                (\s ->
                    if Regex.contains (Regex.regex "\\end{.*}") s then
                        fail "\\end is not allowed here"
                    else
                        succeed s
                )
        )
        (succeed ())


wordsExceptEnd : Parser (List String)
wordsExceptEnd =
    repeat zeroOrMore wordExceptEnd


stringExceptEnd : Parser String
stringExceptEnd =
    map (String.join " ") wordsExceptEnd



{- END ILIAS' CODE -}
