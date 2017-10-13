module LatexParser.Accumulator exposing (getSectionNumber, transformParagraphs, processParagraph, processParagraph2)

import Dict
import LatexParser.Render as Render
import LatexParser.Parser
import String.Extra
import Document.Differ as Differ
import LatexParser.Render as Render exposing (LatexState)
import LatexParser.Latex as Latex
import List.Extra
import Regex


transformParagraphs : List String -> List String
transformParagraphs paragraphs =
    paragraphs
        |> accumulator Render.parseParagraph renderParagraph updateState
        |> Tuple.first


accumulator :
    (a -> List LatexParser.Parser.Latex)
    -> (List LatexParser.Parser.Latex -> LatexState -> b)
    -> (List LatexParser.Parser.Latex -> LatexState -> LatexState)
    -> List a
    -> ( List b, LatexState )
accumulator preprocessor processor stateUpdater inputList =
    inputList
        |> List.foldl (transformer preprocessor processor stateUpdater) ( [], Render.emptyLatexState )


transformer :
    (a -> b)
    -> (b -> c -> d)
    -> (b -> c -> c)
    -> a
    -> ( List d, c )
    -> ( List d, c )
transformer pp f g x acc =
    let
        ( a, b ) =
            acc

        y =
            pp x

        z =
            g y b
    in
        ( a ++ [ f y z ], z )


type alias LatexInfo =
    { typ : String, name : String, value : List String }


getAt : Int -> List String -> String
getAt k list_ =
    List.Extra.getAt k list_ |> Maybe.withDefault "xxx"


info : LatexParser.Parser.Latex -> LatexInfo
info latexElement =
    case latexElement of
        LatexParser.Parser.Macro v ->
            { typ = "macro", name = v.name, value = v.args }

        LatexParser.Parser.Environment v ->
            { typ = "env", name = v.env, value = [ v.body ] }

        _ ->
            { typ = "null", name = "null", value = [] }


handleEquationNumbers : LatexState -> LatexInfo -> LatexState
handleEquationNumbers latexState headElement =
    let
        label =
            headElement.value
                |> List.head
                |> Maybe.withDefault ""
                |> String.trim
                |> Latex.getLabel

        newEqno =
            latexState.eqno + 1

        newDict =
            if label /= "" then
                Dict.insert label ((toString latexState.s1) ++ "." ++ (toString newEqno)) latexState.dict
            else
                latexState.dict
    in
        { latexState | eqno = newEqno, dict = newDict }


handleTheoremNumbers : LatexState -> LatexInfo -> LatexState
handleTheoremNumbers latexState headElement =
    let
        label =
            headElement.value
                |> List.head
                |> Maybe.withDefault ""
                |> String.trim
                |> Latex.getLabel

        newTno =
            latexState.tno + 1

        newDict =
            if label /= "" then
                Dict.insert label ((toString latexState.s1) ++ "." ++ (toString newTno)) latexState.dict
            else
                latexState.dict
    in
        { latexState | tno = newTno, dict = newDict }


updateState : List LatexParser.Parser.Latex -> LatexState -> LatexState
updateState parsedParagraph latexState =
    let
        headElement =
            parsedParagraph
                |> List.head
                |> Maybe.map info
                |> Maybe.withDefault (LatexInfo "null" "null" [])

        newLatexState =
            case ( headElement.typ, headElement.name ) of
                ( "macro", "setcounter" ) ->
                    let
                        args =
                            headElement.value

                        arg1 =
                            getAt 0 args

                        arg2 =
                            getAt 1 args

                        initialSectionNumber =
                            if arg1 == "section" then
                                arg2 |> String.toInt |> Result.withDefault 0
                            else
                                -1
                    in
                        if initialSectionNumber > -1 then
                            { latexState | s1 = initialSectionNumber - 1, s2 = 0, s3 = 0 }
                        else
                            latexState

                ( "macro", "section" ) ->
                    { latexState | s1 = latexState.s1 + 1, s2 = 0, s3 = 0 }

                ( "macro", "subsection" ) ->
                    { latexState | s2 = latexState.s2 + 1, s3 = 0 }

                ( "macro", "subsubsection" ) ->
                    { latexState | s3 = latexState.s3 + 1 }

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


processParagraph : String -> LatexState -> List LatexParser.Parser.Latex
processParagraph paragraph latexState =
    paragraph
        |> (updateSection latexState)
        |> Render.parseParagraph


processParagraph2 : String -> LatexState -> String
processParagraph2 paragraph latexState =
    paragraph
        |> (updateSection latexState)
        |> Render.transformText


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
    if String.contains "\\section" paragraph then
        paragraph
            |> String.Extra.replace "\\section{" ("\\section{" ++ (toString (latexState.s1 + 1)) ++ " ")
    else if String.contains "\\subsection" paragraph then
        paragraph
            |> String.Extra.replace "\\subsection{" ("\\subsection{" ++ (toString latexState.s1) ++ "." ++ (toString (latexState.s2 + 1)) ++ " ")
    else if String.contains "\\subsubsection" paragraph then
        paragraph
            |> String.Extra.replace "\\subsubsection{" ("\\subsubsection{" ++ (toString latexState.s1) ++ "." ++ (toString latexState.s2) ++ "." ++ (toString (latexState.s3 + 1)) ++ " ")
    else
        paragraph


renderParagraph : List LatexParser.Parser.Latex -> LatexState -> String
renderParagraph parsedParagraph latexState =
    parsedParagraph |> (Render.render latexState)
