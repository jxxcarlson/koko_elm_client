module LatexParser.Latex
    exposing
        ( Environment_
        , Macro_
        , InlineMath_
        , DisplayMath_
        , BareMacro_
        , Words_
        , macro
        , environment
        , inlineMath
        , displayMath
        , displayMath2
        , word
        , words
        , texComment
        , ws
        )

{-| XParser parses an as-yet undetermined subset LaTeX
of LaTeX into elements like

Macro1, e.g., a "\foo{bar} "
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



-- |. spaces
--  |= keep oneOrMore (\c -> c /= ' ')


{-| Not currently used
-}
type alias Words_ =
    { value : List String
    }


{-| Not currently used
-}
words : Parser Words_
words =
    inContext "words" <|
        succeed Words_
            |. spaces
            |= repeat zeroOrMore word
            |. oneOf [ symbol "\n", symbol "\\" ]


type alias Environment_ =
    { env : String
    , body : String
    }


{-| run environment "\begin{foo}Blah, blah ...\end{foo} "
-}
environment : Parser Environment_
environment =
    inContext "environment" <|
        delayedCommit (keyword "\\begin") <|
            succeed Environment_
                |. symbol "{"
                |= keep zeroOrMore (\c -> c /= '}')
                |. symbol "}"
                |= keep zeroOrMore (\c -> c /= '\\')
                |. symbol "\\"
                |. (keyword "end")
                |. symbol "{"
                |. keep zeroOrMore (\c -> c /= '}')
                |. symbol "}"
                --|. ignore (Exactly 1) (\c -> c == ' ' || c == '\n')
                |. ignore zeroOrMore (\c -> c == ' ' || c == '\n')



-- |. spaces
-- |. repeat zeroOrMore (oneOf [ symbol "\n" ])
-- spaces


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
            -- |. ignore (Exactly 1) (\c -> c == ' ' || c == '\n')
            -- |. spaces
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
                --|. ignore (Exactly 1) (\c -> c == ' ' || c == '\n')
                --|. spaces
                |. ignore oneOrMore (\c -> c == ' ' || c == '\n')



-- \[ \int_0^1 x^n dx = \frac{1}{n+1} \]


displayMath2 : Parser DisplayMath_
displayMath2 =
    inContext "display math" <|
        delayedCommit (symbol "\\[") <|
            succeed
                DisplayMath_
                -- |. ignore zeroOrMore (\c -> c == ' ' || c == '\n')
                |= keep zeroOrMore (\c -> c /= '\\')
                |. symbol "\\]"
                |. ignore (Exactly 1) (\c -> c == ' ' || c == '\n')
                |. spaces



-- |. ignore oneOrMore (\c -> c == ' ' || c == '\n')
-- |. ignore oneOrMore (\c -> c == ' ' || c == '\n')


arg : Parser String
arg =
    succeed identity
        |. symbol "{"
        |= keep zeroOrMore (\c -> c /= '}')
        |. symbol "}"


type alias Macro_ =
    { name : String
    , args : List String
    }


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


type alias BareMacro_ =
    { name : String
    }


{-| Not used yet
-}
bareMacro : Parser BareMacro_
bareMacro =
    inContext "bare macro" <|
        succeed BareMacro_
            |. symbol "\\"
            |= keep zeroOrMore (\c -> (c /= ' ' || c /= '\n') && (c /= '{'))


texComment : Parser ()
texComment =
    symbol "%"
        |. ignoreUntil "\n"
        |. ignore zeroOrMore (\c -> c == ' ' || c == '\n')



{- Below this line: stuff I might use -}


lowVar : Parser String
lowVar =
    variable Char.isLower isVarChar keywords


capVar : Parser String
capVar =
    variable Char.isUpper isVarChar keywords


isVarChar : Char -> Bool
isVarChar char =
    Char.isLower char
        || Char.isUpper char
        || Char.isDigit char
        || char
        == '_'


keywords : Set.Set String
keywords =
    Set.fromList [ "begin", "end" ]
