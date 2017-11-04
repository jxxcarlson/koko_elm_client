module MiniLatex.Accumulator
    exposing
        ( accumulator
        , updateState
        , renderParagraph
        , getSectionNumber
        , transformParagraphs
        , processParagraph
        , processParagraph2
        )

import Dict
import MiniLatex.Parser as Parser exposing (macro, defaultLatexList, parseParagraph, LatexExpression(..))
import String.Extra
import Document.Differ as Differ
import MiniLatex.Render as Render exposing (render, renderLatexList)
import MiniLatex.LatexState exposing (..)
import MiniLatex.ParserTools as PT
import List.Extra
import Regex
import Parser as P


getElement : Int -> List LatexExpression -> String
getElement k list =
    let
        lxString =
            List.Extra.getAt k list |> Maybe.withDefault (LXString "xxx")
    in
        case lxString of
            LXString str ->
                str

            _ ->
                "yyy"


transformParagraphs : List String -> List String
transformParagraphs paragraphs =
    paragraphs
        |> accumulator Parser.parseParagraph renderParagraph updateState
        |> Tuple.first


accumulator :
    (String -> List LatexExpression)
    -> (List LatexExpression -> LatexState -> String)
    -> (List LatexExpression -> LatexState -> LatexState)
    -> List String
    -> ( List String, LatexState )
accumulator parse render updateState inputList =
    inputList
        |> List.foldl (transformer parse render updateState) ( [], emptyLatexState )


transformer :
    (input -> parsedInput) -- parse
    -> (parsedInput -> state -> renderedInput) -- render
    -> (parsedInput -> state -> state) -- updateState
    -> input -- Here is it a string
    -> ( List renderedInput, state ) -- acc
    -> ( List renderedInput, state ) -- acc
transformer parse render updateState input acc =
    let
        ( outputList, state ) =
            acc

        parsedInput =
            parse input

        newState =
            updateState parsedInput state
    in
        ( outputList ++ [ render parsedInput newState ], newState )


type alias LatexInfo =
    { typ : String, name : String, value : List LatexExpression }


getAt : Int -> List String -> String
getAt k list_ =
    List.Extra.getAt k list_ |> Maybe.withDefault "xxx"


info : LatexExpression -> LatexInfo
info latexExpression =
    case latexExpression of
        Macro name args ->
            { typ = "macro", name = name, value = args }

        Environment name body ->
            { typ = "env", name = name, value = [ body ] }

        _ ->
            { typ = "null", name = "null", value = [] }


getLabel2 str =
    let
        maybeMacro =
            str
                |> String.trim
                |> P.run Parser.macro
    in
        case maybeMacro of
            Ok macro ->
                macro |> PT.getFirstMacroArg "label"

            _ ->
                ""


handleEquationNumbers : LatexState -> LatexInfo -> LatexState
handleEquationNumbers latexState info =
    let
        {- label =
           info.value
               |> List.head
               |> Maybe.withDefault (Macro "NULL" [])
               |> PT.getFirstMacroArg "label"
        -}
        data =
            info.value
                |> List.head
                |> Maybe.withDefault (Macro "NULL" [])

        label =
            case data of
                LXString str ->
                    getLabel2 str

                _ ->
                    ""

        latexState1 =
            incrementCounter "eqno" latexState

        eqno =
            getCounter "eqno" latexState1

        s1 =
            getCounter "s1" latexState1

        latexState2 =
            if label /= "" then
                setCrossReference label ((toString s1) ++ "." ++ (toString eqno)) latexState1
            else
                latexState1
    in
        latexState2


handleTheoremNumbers : LatexState -> LatexInfo -> LatexState
handleTheoremNumbers latexState info =
    let
        label =
            info.value
                |> List.head
                |> Maybe.withDefault (Macro "NULL" [])
                |> PT.getFirstMacroArg "label"

        latexState1 =
            incrementCounter "tno" latexState

        tno =
            getCounter "tno" latexState1

        s1 =
            getCounter "s1" latexState1

        latexState2 =
            if label /= "" then
                setCrossReference label ((toString s1) ++ "." ++ (toString tno)) latexState1
            else
                latexState1
    in
        latexState2


updateState : List LatexExpression -> LatexState -> LatexState
updateState parsedParagraph latexState =
    let
        headElement =
            parsedParagraph
                |> List.head
                |> Maybe.map info
                |> Maybe.withDefault (LatexInfo "null" "null" [ (Macro "null" []) ])

        newLatexState =
            case ( headElement.typ, headElement.name ) of
                ( "macro", "setcounter" ) ->
                    let
                        args =
                            headElement.value

                        arg1 =
                            getElement 0 args

                        arg2 =
                            getElement 1 args

                        initialSectionNumber =
                            if arg1 == "section" then
                                arg2 |> String.toInt |> Result.withDefault 0
                            else
                                -1
                    in
                        if initialSectionNumber > -1 then
                            latexState
                                |> updateCounter "s1" (initialSectionNumber - 1)
                                |> updateCounter "s2" 0
                                |> updateCounter "s3" 0
                        else
                            latexState

                ( "macro", "section" ) ->
                    latexState
                        |> incrementCounter "s1"
                        |> updateCounter "s2" 0
                        |> updateCounter "s3" 0

                ( "macro", "subsection" ) ->
                    latexState
                        |> incrementCounter "s2"
                        |> updateCounter "s3" 0

                ( "macro", "subsubsection" ) ->
                    latexState
                        |> incrementCounter "s3"

                ( "env", "theorem" ) ->
                    handleTheoremNumbers latexState headElement

                ( "env", "proposition" ) ->
                    handleTheoremNumbers latexState headElement

                ( "env", "lemma" ) ->
                    handleTheoremNumbers latexState headElement

                ( "env", "definition" ) ->
                    handleTheoremNumbers latexState headElement

                ( "env", "corollary" ) ->
                    handleTheoremNumbers latexState headElement

                ( "env", "equation" ) ->
                    handleEquationNumbers latexState headElement

                ( "env", "align" ) ->
                    handleEquationNumbers latexState headElement

                _ ->
                    latexState
    in
        newLatexState


processParagraph : String -> LatexState -> List LatexExpression
processParagraph paragraph latexState =
    paragraph
        |> (updateSection latexState)
        |> parseParagraph


processParagraph2 : String -> LatexState -> String
processParagraph2 paragraph latexState =
    paragraph
        |> (updateSection latexState)
        |> parseParagraph
        |> renderLatexList latexState


getSectionNumber text =
    let
        rx =
            (Regex.regex "\\\\setcounter{section}{(.*)}")

        result =
            Regex.find (Regex.AtMost 1) rx text
                |> List.map .submatches
                |> List.head
                |> Maybe.withDefault [ Just "-1" ]
                |> List.head
                |> Maybe.withDefault (Just "-1")

        index =
            case result of
                Just n ->
                    String.toInt n |> Result.withDefault -1

                Nothing ->
                    -1
    in
        index


updateSection : LatexState -> String -> String
updateSection latexState paragraph =
    let
        s1 =
            getCounter "s1" latexState

        s2 =
            getCounter "s2" latexState

        s3 =
            getCounter "s3" latexState
    in
        if String.contains "\\section" paragraph then
            paragraph
                |> String.Extra.replace "\\section{" ("\\section{" ++ (toString (s1 + 1)) ++ " ")
        else if String.contains "\\subsection" paragraph then
            paragraph
                |> String.Extra.replace "\\subsection{" ("\\subsection{" ++ (toString s1) ++ "." ++ (toString (s2 + 1)) ++ " ")
        else if String.contains "\\subsubsection" paragraph then
            paragraph
                |> String.Extra.replace "\\subsubsection{" ("\\subsubsection{" ++ (toString s1) ++ "." ++ (toString s2) ++ "." ++ (toString (s3 + 1)) ++ " ")
        else
            paragraph


renderParagraph : List LatexExpression -> LatexState -> String
renderParagraph parsedParagraph latexState =
    renderLatexList latexState parsedParagraph
        |> \paragraph -> "<p>" ++ paragraph ++ "</p>"
