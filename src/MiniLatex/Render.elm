module MiniLatex.Render exposing (..)

import MiniLatex.Parser exposing (LatexExpression(..))
import Dict
import List.Extra
import Parser


type alias CrossReferences =
    Dict.Dict String String


emptyDict =
    Dict.empty


type alias LatexState =
    { s1 : Int, s2 : Int, s3 : Int, tno : Int, eqno : Int, dict : CrossReferences }


emptyLatexState =
    { s1 = 0, s2 = 0, s3 = 0, tno = 0, eqno = 0, dict = Dict.empty }


parseString parser str =
    Parser.run parser str


renderString parser str =
    let
        parserOutput =
            Parser.run parser str

        renderOutput =
            case parserOutput of
                Ok latexExpression ->
                    render emptyLatexState latexExpression

                Err _ ->
                    "PARSE ERROR"
    in
        renderOutput


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
            args |> List.map (render latexState) |> String.join (" ")

        LXString str ->
            str


getElement : Int -> List LatexExpression -> LatexExpression
getElement k list =
    List.Extra.getAt k list |> Maybe.withDefault (LXString "xxx")


renderItem : LatexState -> Int -> LatexExpression -> String
renderItem latexState level latexExpression =
    ""


renderMacro : LatexState -> String -> List LatexExpression -> String
renderMacro latexState name args =
    case name of
        "italic" ->
            renderItalic latexState args

        _ ->
            "Unresolved macro: " ++ name


renderComment : String -> String
renderComment str =
    ""


renderItalic : LatexState -> List LatexExpression -> String
renderItalic latexState args =
    let
        arg =
            getElement 0 args

        r =
            render latexState arg
    in
        "<it>" ++ r ++ "</it>"
