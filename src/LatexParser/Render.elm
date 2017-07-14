module LatexParser.Render exposing (..)

import LatexParser.Parser exposing (Latex(..), latex, latexList, latexListGet)
import List.Extra
import Parser


transformText text =
    Parser.run latexList (text ++ " ")
        |> latexListGet
        |> List.map transformLatex
        |> String.join (" ")


getAt : Int -> List String -> String
getAt k list_ =
    List.Extra.getAt k list_ |> Maybe.withDefault "xxx"


transformLatex : Latex -> String
transformLatex latex =
    case latex of
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


handleEnvironment v =
    let
        env =
            v.env

        body =
            v.body
    in
        case env of
            _ ->
                handleDefaultEnvironment env body


handleDefaultEnvironment env body =
    "<h4>" ++ env ++ "</h4>\n" ++ body ++ "\n"



-- MACROS


handleMacro v =
    case v.name of
        "emph" ->
            handleEmph v.args

        _ ->
            "Macro <strong>" ++ v.name ++ ":</strong> not recognized"


handleEmph args =
    let
        arg =
            getAt 0 args
    in
        "<it>" ++ arg ++ "</it>"
