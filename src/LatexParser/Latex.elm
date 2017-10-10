module LatexParser.Latex exposing (..)

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

-- ( DisplayMath_
-- , Environment_
-- , InlineMath_
-- , Macro_
-- , displayMath
-- , displayMath2
-- , environment
-- , inlineMath
-- , macro
-- , texComment
-- , word
-- , ws
-- )
{- This version contains Ilias' improved code -}

import Parser exposing (..)


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


type alias Environment_ =
    { env : String
    , body : String
    }


type alias InlineMath_ =
    { value : String
    }


{-| }
Re "source", see the Parser documentation. It is defined and
explained there.
-}
parseUntil : String -> Parser String
parseUntil marker =
    ignoreUntil marker
        |> source
        |> map (String.dropRight <| String.length marker)


inlineMath : Parser InlineMath_
inlineMath =
    inContext "inline math" <|
        succeed InlineMath_
            |. symbol "$"
            |= parseUntil "$"
            |. ws


type alias DisplayMath_ =
    { value : String
    }


displayMathOLD : Parser DisplayMath_
displayMathOLD =
    inContext "display math" <|
        delayedCommit (symbol "$$") <|
            succeed DisplayMath_
                |= parseUntil "$$"
                |. ignore oneOrMore (\c -> c == ' ' || c == '\n')


displayMath : Parser DisplayMath_
displayMath =
    inContext "display math" <|
        succeed DisplayMath_
            |. ignore zeroOrMore ((==) ' ')
            |. symbol "$$"
            |= parseUntil "$$"


displayMath2OLD : Parser DisplayMath_
displayMath2OLD =
    inContext "display math" <|
        delayedCommit (symbol "\\[") <|
            succeed DisplayMath_
                |= parseUntil "\\]"
                |. ignore (Exactly 1) (\c -> c == ' ' || c == '\n')
                |. spaces


displayMath2 : Parser DisplayMath_
displayMath2 =
    inContext "display math" <|
        succeed DisplayMath_
            |. ignore zeroOrMore ((==) ' ')
            |. symbol "\\["
            |= parseUntil "\\]"


type alias Macro_ =
    { name : String
    , args : List String
    }


arg : Parser String
arg =
    succeed identity
        |. symbol "{"
        |= parseUntil "}"


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
            |. ws



--|. oneOf [ ignore (Exactly 1) (\c -> c == ' ' || c == '\n'), Parser.end ]
-- |. oneOf [ ignore (Exactly 1) (\c -> c == ' ' || c == '\n' || c == ',' || c == '.' || c == ';' || c == '!' || c == '?'), Parser.end ]
-- |. ws


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
    let
        endWord =
            "\\end{" ++ envType ++ "}"
    in
        ignoreUntil endWord
            |> source
            |> map (String.dropRight (String.length endWord))
            |> map (Environment_ envType)


beginWord : Parser String
beginWord =
    succeed identity
        |. ignore zeroOrMore ((==) ' ')
        |. symbol "\\begin{"
        |= parseUntil "}"
