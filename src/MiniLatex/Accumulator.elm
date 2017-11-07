module MiniLatex.Accumulator
    exposing
        ( accumulator
        , updateState
        , renderParagraph
        , getSectionNumber
        , transformParagraphs
        , parseParagraphs
        , renderParagraphs
        , processParagraph
        , processParagraph2
        )

import MiniLatex.Parser as Parser exposing (macro, defaultLatexList, parseParagraph, LatexExpression(..))
import String.Extra
import MiniLatex.Differ as Differ exposing (EditRecord)
import MiniLatex.Render as Render exposing (render, renderLatexList)
import MiniLatex.LatexState exposing (..)
import MiniLatex.ParserTools as PT
import List.Extra
import Regex
import Parser as P


getAt : Int -> List String -> String
getAt k list_ =
    List.Extra.getAt k list_ |> Maybe.withDefault "xxx"


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



-- |> Tuple.first


transformParagraphs : LatexState -> List String -> ( List String, LatexState )
transformParagraphs latexState paragraphs =
    paragraphs
        |> accumulator Parser.parseParagraph renderParagraph updateState latexState


accumulator :
    (String -> List LatexExpression) -- parse
    -> (List LatexExpression -> LatexState -> String) -- render
    -> (List LatexExpression -> LatexState -> LatexState) -- updateState
    -> LatexState -- latexState
    -> List String
    -> ( List String, LatexState )
accumulator parse render updateState latexState inputList =
    inputList
        |> List.foldl (transformer parse render updateState) ( [], latexState )


transformer :
    (String -> List LatexExpression) -- parse
    -> (List LatexExpression -> LatexState -> String) -- render
    -> (List LatexExpression -> LatexState -> LatexState) -- updateState
    -> String --input
    -> ( List String, LatexState ) -- acc
    -> ( List String, LatexState ) -- acc
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



{- Accumulator1: parse a list of paragraphs and return
   a tuple consisting of a list of (List LatexExpression)
   and the computed LatexState

-}


parseParagraphs : LatexState -> List String -> ( List (List LatexExpression), LatexState )
parseParagraphs latexState paragraphs =
    paragraphs
        |> accumulator1 Parser.parseParagraph updateState latexState


accumulator1 :
    (String -> List LatexExpression) -- parse
    -> (List LatexExpression -> LatexState -> LatexState) -- updateState
    -> LatexState -- latexState
    -> List String
    -> ( List (List LatexExpression), LatexState )
accumulator1 parse updateState latexState inputList =
    inputList
        |> List.foldl (transformer1 parse updateState) ( [], latexState )


transformer1 :
    (String -> List LatexExpression) -- parse
    -> (List LatexExpression -> LatexState -> LatexState) -- updateState
    -> String --input
    -> ( List (List LatexExpression), LatexState ) -- acc
    -> ( List (List LatexExpression), LatexState ) -- acc
transformer1 parse updateState input acc =
    let
        ( outputList, state ) =
            acc

        parsedInput =
            parse input

        newState =
            updateState parsedInput state
    in
        ( outputList ++ [ parsedInput ], newState )



{- renderParagraphs: take a list of (List LatexExpressions)
   and a LatexState and rehder the list into a list of strings.
-}


renderParagraphs : LatexState -> List (List LatexExpression) -> ( List String, LatexState )
renderParagraphs latexState paragraphs =
    paragraphs
        |> accumulator2 renderLatexList updateState latexState


accumulator2 :
    (LatexState -> List LatexExpression -> String) -- render
    -> (List LatexExpression -> LatexState -> LatexState) -- updateState
    -> LatexState -- latexState
    -> List (List LatexExpression)
    -> ( List String, LatexState )
accumulator2 render updateState latexState inputList =
    inputList
        |> List.foldl (transformer2 render updateState) ( [], latexState )


transformer2 :
    (LatexState -> List LatexExpression -> String) -- render
    -> (List LatexExpression -> LatexState -> LatexState) -- updateState
    -> List LatexExpression --input
    -> ( List String, LatexState ) -- acc
    -> ( List String, LatexState ) -- acc
transformer2 render updateState input acc =
    let
        ( outputList, state ) =
            acc

        renderedInput =
            render state input

        newState =
            updateState input state

        _ =
            Debug.log "state" newState
    in
        ( outputList ++ [ renderedInput ], newState )


type alias LatexInfo =
    { typ : String, name : String, value : List LatexExpression }


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

        _ =
            Debug.log "headElement" headElement

        he =
            { typ = "macro", name = "setcounter", value = [ LatexList ([ LXString "section" ]), LatexList ([ LXString "7" ]) ] }

        newLatexState =
            case ( headElement.typ, headElement.name ) of
                ( "macro", "setcounter" ) ->
                    let
                        argList =
                            headElement.value |> List.map PT.latexList2List |> List.map PT.list2LeadingString

                        arg1 =
                            getAt 0 argList

                        arg2 =
                            getAt 1 argList

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
