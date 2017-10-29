module MiniLatex.Render exposing (..)

import MiniLatex.Parser exposing (LatexExpression(..))
import Dict


type alias CrossReferences =
    Dict.Dict String String


emptyDict =
    Dict.empty


type alias LatexState =
    { s1 : Int, s2 : Int, s3 : Int, tno : Int, eqno : Int, dict : CrossReferences }


emptyLatexState =
    { s1 = 0, s2 = 0, s3 = 0, tno = 0, eqno = 0, dict = Dict.empty }


render : LatexState -> LatexExpression -> String
render latexState latexExpression =
    case latexExpression of
        Comment str ->
            renderComment str

        Macro name args ->
            renderMacro latexState name args

        Item level latexExpression ->
            renderItem latexState level latexExpression

        InlineMath str ->
            "$" ++ str ++ "$"

        DisplayMath str ->
            "$$" ++ str ++ "$$"

        Environment name args ->
            "DUMMY"

        LatexList args ->
            "DUMMY"

        LXString str ->
            str


renderItem : LatexState -> Int -> LatexExpression -> String
renderItem latexState level latexExpression =
    ""


renderMacro : LatexState -> String -> List String -> String
renderMacro latexState name args =
    case name of
        "italic" ->
            renderItalic args

        _ ->
            "Macro " ++ name ++ (String.join ", " args)


renderComment : String -> String
renderComment str =
    ""


renderItalic : List String -> String
renderItalic args =
    ""
