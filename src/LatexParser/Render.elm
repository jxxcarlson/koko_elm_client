module LatexParser.Render exposing (..)

import LatexParser.Parser exposing (Latex(..), latex, latexList)
import List.Extra


getAt : Int -> List String -> String
getAt k list_ =
    List.Extra.getAt k list_ |> Maybe.withDefault "xxx"


transform : Latex -> String
transform latex =
    case latex of
        Word str ->
            str

        Macro v ->
            handleMacro v

        _ ->
            "ERR"


handleMacro v =
    case v.name of
        "emph" ->
            handleEmph v.args

        _ ->
            "Macro " ++ v.name ++ ": not recognized"


handleEmph args =
    let
        arg =
            getAt 0 args
    in
        "<it>" ++ arg ++ "</it>"
