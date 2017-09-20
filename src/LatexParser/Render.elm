module LatexParser.Render exposing (..)

import LatexParser.Parser exposing (Latex(..), latex, latexList, latexListGet)
import List.Extra
import String.Extra
import Parser


transformText : String -> String
transformText text =
    let
        _ =
            Debug.log "transformText" text

        transformedText =
            Parser.run latexList text
                |> latexListGet
                |> List.map transformLatex
                |> String.join (" ")
    in
        transformedText


getAt : Int -> List String -> String
getAt k list_ =
    List.Extra.getAt k list_ |> Maybe.withDefault "xxx"


transformLatex : Latex -> String
transformLatex latex =
    case latex of
        Comment () ->
            ""

        Word str ->
            str

        Macro v ->
            handleMacro v

        Environment v ->
            handleEnvironment v

        InlineMath v ->
            " $" ++ v.value ++ "$ "

        DisplayMath v ->
            "\n$$\n" ++ v.value ++ "\n$$\n"

        _ ->
            "ERR"



-- ENVIRONMENTS


handleEnvironment : { a | body : String, env : String } -> String
handleEnvironment v =
    let
        env =
            v.env

        body =
            v.body
    in
        case env of
            "equation" ->
                handleEquationEnvironment body

            _ ->
                handleDefaultEnvironment env body


handleEquationEnvironment : String -> String
handleEquationEnvironment body =
    "\n\\begin{equation}\n" ++ body ++ "\n\\end{equation}\n"


handleDefaultEnvironment : String -> String -> String
handleDefaultEnvironment env body =
    "\n<strong>" ++ (String.Extra.toSentenceCase env) ++ "</strong>\n<it>\n" ++ body ++ "\n</it>\n"



-- MACROS


handleMacro : { a | args : List String, name : String } -> String
handleMacro v =
    case v.name of
        "emph" ->
            handleEmph v.args

        _ ->
            "Macro <b>" ++ v.name ++ ":</b> not recognized"


handleEmph : List String -> String
handleEmph args =
    let
        arg =
            getAt 0 args
    in
        "<b>" ++ arg ++ "</b>"
