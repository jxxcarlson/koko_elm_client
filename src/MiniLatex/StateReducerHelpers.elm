module MiniLatex.StateReducerHelpers exposing (..)

import List.Extra
import MiniLatex.LatexState
    exposing
        ( Counters
        , CrossReferences
        , LatexState
        , addSection
        , getCounter
        , incrementCounter
        , setCrossReference
        , setDictionaryItem
        , updateCounter
        )
import MiniLatex.Parser as Parser exposing (LatexExpression(..))
import MiniLatex.ParserTools as PT
import Parser as P


type alias LatexInfo =
    { typ : String, name : String, value : List LatexExpression }


macroSetCounter : LatexInfo -> LatexState -> LatexState
macroSetCounter headElement latexState =
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


macroSection : LatexInfo -> LatexState -> LatexState
macroSection headElement latexState =
    let
        label =
            getCounter "s1" latexState |> (\x -> x + 1) |> toString
    in
    latexState
        |> incrementCounter "s1"
        |> updateCounter "s2" 0
        |> updateCounter "s3" 0
        |> addSection (PT.unpackString headElement.value) label 1


macroSubsection : LatexInfo -> LatexState -> LatexState
macroSubsection headElement latexState =
    let
        s1 =
            getCounter "s1" latexState |> toString

        s2 =
            getCounter "s2" latexState |> (\x -> x + 1) |> toString

        label =
            s1 ++ "." ++ s2
    in
    latexState
        |> incrementCounter "s2"
        |> updateCounter "s3" 0
        |> addSection (PT.unpackString headElement.value) label 2


macroSubsubsection : LatexInfo -> LatexState -> LatexState
macroSubsubsection headElement latexState =
    let
        s1 =
            getCounter "s1" latexState |> toString

        s2 =
            getCounter "s2" latexState |> toString

        s3 =
            getCounter "s3" latexState |> (\x -> x + 1) |> toString

        label =
            s1 ++ "." ++ s2 ++ "." ++ s3
    in
    latexState
        |> incrementCounter "s3"
        |> addSection (PT.unpackString headElement.value) label 2


macroTitle : LatexInfo -> LatexState -> LatexState
macroTitle headElement latexState =
    setDictionaryItemForMacro "title" headElement latexState


macroAuthor : LatexInfo -> LatexState -> LatexState
macroAuthor headElement latexState =
    setDictionaryItemForMacro "author" headElement latexState


macroDate : LatexInfo -> LatexState -> LatexState
macroDate headElement latexState =
    setDictionaryItemForMacro "date" headElement latexState


macroEmail : LatexInfo -> LatexState -> LatexState
macroEmail headElement latexState =
    setDictionaryItemForMacro "email" headElement latexState


macroRevision : LatexInfo -> LatexState -> LatexState
macroRevision headElement latexState =
    setDictionaryItemForMacro "revision" headElement latexState


envTheorem : LatexInfo -> LatexState -> LatexState
envTheorem headElement latexState =
    handleTheoremNumbers latexState headElement


envProposition : LatexInfo -> LatexState -> LatexState
envProposition headElement latexState =
    handleTheoremNumbers latexState headElement


envLemma : LatexInfo -> LatexState -> LatexState
envLemma headElement latexState =
    handleTheoremNumbers latexState headElement


envDefinition : LatexInfo -> LatexState -> LatexState
envDefinition headElement latexState =
    handleTheoremNumbers latexState headElement


envCorollary : LatexInfo -> LatexState -> LatexState
envCorollary headElement latexState =
    handleTheoremNumbers latexState headElement


envEquation : LatexInfo -> LatexState -> LatexState
envEquation headElement latexState =
    handleEquationNumbers latexState headElement


envAlign : LatexInfo -> LatexState -> LatexState
envAlign headElement latexState =
    handleEquationNumbers latexState headElement



{- Handlers -}


setDictionaryItemForMacro macroname headElement latexState =
    let
        value =
            PT.unpackString headElement.value
    in
    setDictionaryItem macroname value latexState


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
                    getLabel str

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
                setCrossReference label (toString s1 ++ "." ++ toString eqno) latexState1
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
                setCrossReference label (toString s1 ++ "." ++ toString tno) latexState1
            else
                latexState1
    in
    latexState2



{- Helpers  I -}


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


getLabel str =
    let
        maybeMacro =
            str
                |> String.trim
                |> P.run (Parser.macro Parser.ws)
    in
    case maybeMacro of
        Ok macro ->
            macro |> PT.getFirstMacroArg "label"

        _ ->
            ""