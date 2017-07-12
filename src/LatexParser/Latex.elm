module LatexParser.Latex
    exposing
        ( M1
        , M2
        , Env
        , macro1
        , macro2
        , environment
        )

{-|

XParser parses an as-yet undetermined subset LaTeX
of LaTeX into elements like

  Macro1, e.g., a "\foo{bar} "
  Macro2, e.g., a "\foo{bar}{baz}"
  Environment, e.g., a "\begin{ENV} BODY \end{ENV}"
  InlineMath, e.g., a "$ a^2 + b^2 + c^2 $"
  DisplayMpath, e.g., a "\[ a^2 + b^2 + c^2 \]"

The parsed text will be used to construct a mixture
of HTML, inlne LateX, and display LaTeX that can
be rendered by a browser + MathJax.

Recall that MathJax does not process other than inlne and
display LateX.  The aim, therefore, is to properly render
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


type alias Env =
    { env : String
    , body : String
    }


{-|
   run environment "\\begin{foo}Blah, blah ...\\end{foo} "
-}
environment : Parser Env
environment =
    succeed Env
        |. symbol "\\"
        |. (keyword "begin")
        |. symbol "{"
        |= keep zeroOrMore (\c -> c /= '}')
        |. symbol "}"
        |= keep zeroOrMore (\c -> c /= '\\')
        |. symbol "\\"
        |. (keyword "end")
        |. symbol "{"
        |. keep zeroOrMore (\c -> c /= '}')
        |. symbol "}"
        |. ignore (Exactly 1) (\c -> c == ' ')


type alias M1 =
    { name : String
    , arg : String
    }


{-|
   run macro1 "\\foo{bar} "
-}
macro1 : Parser M1
macro1 =
    succeed M1
        |. symbol "\\"
        |= keep zeroOrMore (\c -> c /= '{')
        |. symbol "{"
        |= keep zeroOrMore (\c -> c /= '}')
        |. symbol "}"
        |. ignore (Exactly 1) (\c -> c == ' ')


type alias M2 =
    { name : String
    , arg1 : String
    , arg2 : String
    }


{-|
   run macro2 "\\foo{bar}{baz} "
-}
macro2 : Parser M2
macro2 =
    succeed M2
        |. symbol "\\"
        |= keep zeroOrMore (\c -> c /= '{')
        |. symbol "{"
        |= keep zeroOrMore (\c -> c /= '}')
        |. symbol "}"
        |. symbol "{"
        |= keep zeroOrMore (\c -> c /= '}')
        |. symbol "}"
        |. ignore (Exactly 1) (\c -> c == ' ')



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


spaces : Parser ()
spaces =
    ignore zeroOrMore (\c -> c == ' ')
