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
            "center" ->
                handleCenterEnvironment body

            "equation" ->
                handleEquationEnvironment body

            "equationalign" ->
                handleEquationAlignEnvironment body

            "verbatim" ->
                handleVerbatim body

            _ ->
                handleDefaultEnvironment env body


handleCenterEnvironment : String -> String
handleCenterEnvironment body =
    "\n<div class=\"center\">\n" ++ body ++ "\n</div>\n"


handleEquationEnvironment : String -> String
handleEquationEnvironment body =
    "\n\\begin{equation}\n" ++ body ++ "\n\\end{equation}\n"


handleEquationAlignEnvironment : String -> String
handleEquationAlignEnvironment body =
    "\n\\begin{equationalign}\n" ++ body ++ "\n\\end{equationalign}\n"


handleDefaultEnvironment : String -> String -> String
handleDefaultEnvironment env body =
    "\n<strong>" ++ (String.Extra.toSentenceCase env) ++ "</strong>\n<it>\n" ++ body ++ "\n</it>\n"


handleVerbatim : String -> String
handleVerbatim body =
    "\n<pre>" ++ body ++ "</pre>\n"



-- MACROS


handleMacro : { a | args : List String, name : String } -> String
handleMacro v =
    let
        _ =
            Debug.log "macro name" v.name

        _ =
            Debug.log "macro args" v.args
    in
        case v.name of
            "code" ->
                handleCode v.args

            "emph" ->
                handleEmph v.args

            "hyperlink" ->
                handleHyperlink v.args

            "image" ->
                handleImage v.args

            "italic" ->
                handleItalic v.args

            "section" ->
                handleSection v.args

            "strong" ->
                handleStrong v.args

            "subsection" ->
                handleSubSection v.args

            "subsubsection" ->
                handleSubSubSection v.args

            "subsubsubsection" ->
                handleSubSubSubSection v.args

            _ ->
                "Macro <b>" ++ v.name ++ ":</b> not recognized"


handleBold : List String -> String
handleBold args =
    let
        arg =
            getAt 0 args
    in
        "<strong>" ++ arg ++ "</strong>"


handleCode : List String -> String
handleCode args =
    let
        arg =
            getAt 0 args
    in
        "<code>" ++ arg ++ "</code>"


handleEmph : List String -> String
handleEmph args =
    let
        arg =
            getAt 0 args
    in
        "<b>" ++ arg ++ "</b>"


handleHyperlink : List String -> String
handleHyperlink args =
    let
        url =
            getAt 0 args

        label =
            getAt 1 args
    in
        "<a href=\"" ++ url ++ " target=_blank\">" ++ label ++ "</a>"


handleImage : List String -> String
handleImage args =
    let
        url =
            getAt 0 args

        label =
            getAt 1 args

        attributes =
            getAt 2 args
    in
        "<image src=\"" ++ url ++ " " ++ attributes ++ " \">"


handleItalic : List String -> String
handleItalic args =
    let
        arg =
            getAt 0 args
    in
        "<span class=italic>" ++ arg ++ "</span>"


handleStrong : List String -> String
handleStrong args =
    let
        arg =
            getAt 0 args
    in
        "<strong>" ++ arg ++ "</strong>"


handleSection : List String -> String
handleSection args =
    let
        arg =
            getAt 0 args
    in
        "<h1>" ++ arg ++ "</h1>"


handleSubSection : List String -> String
handleSubSection args =
    let
        arg =
            getAt 0 args
    in
        "<h2>" ++ arg ++ "</h2>"


handleSubSubSection : List String -> String
handleSubSubSection args =
    let
        arg =
            getAt 0 args
    in
        "<h3>" ++ arg ++ "</h3>"


handleSubSubSubSection : List String -> String
handleSubSubSubSection args =
    let
        arg =
            getAt 0 args
    in
        "<h3>" ++ arg ++ "</h3>"
