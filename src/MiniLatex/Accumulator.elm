module MiniLatex.Accumulator
    exposing
        ( parseParagraphs
        , renderParagraphs
        )

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
import MiniLatex.Parser as Parser exposing (LatexExpression(..), macro, parse)
import MiniLatex.ParserTools as PT
import MiniLatex.Render as Render exposing (renderLatexList)
import Parser as P
import String.Extra


{- Types -}


type alias LatexInfo =
    { typ : String, name : String, value : List LatexExpression }


type alias Reducer a b =
    a -> b -> b


type alias RenderReducer =
    Reducer (List LatexExpression) ( List String, LatexState )


type alias RenderReducerTransformer a b c =
    (a -> b -> c)
    -> Reducer b a
    -> Reducer b ( List c, a )


type alias ParserReducerTransformer a b c =
    (a -> b)
    -> Reducer b c
    -> Reducer a ( List b, c )



{- EXPORTED FUNCTIONS -}
-- transformParagraphs : LatexState -> List String -> ( List String, LatexState )
-- transformParagraphs latexState paragraphs =
--     paragraphs
--         |> accumulator Parser.parse renderParagraph updateStateReducer latexState
--
--
-- renderParagraph : List LatexExpression -> LatexState -> String
-- renderParagraph parsedParagraph latexState =
--     renderLatexList latexState parsedParagraph
--         |> \paragraph -> "<p>" ++ paragraph ++ "</p>"


{-| parseParagraphs: Using a given LatexState, take a list of strings,
i.e., paragraphs, and compute a tuple consisting of the parsed
paragraphs and the upodated LatexState.
-}
parseParagraphs : LatexState -> List String -> ( List (List LatexExpression), LatexState )
parseParagraphs latexState paragraphs =
    parseAccumulator latexState paragraphs


{-| renderParagraphs: take a list of (List LatexExpressions)
and a LatexState and rehder the list into a list of strings.
-}
renderParagraphs : LatexState -> List (List LatexExpression) -> ( List String, LatexState )
renderParagraphs latexState paragraphs =
    renderAccumulator latexState paragraphs



{- ACCUMULATORS AND TRANSFORMERS -}


parseAccumulator : LatexState -> List String -> ( List (List LatexExpression), LatexState )
parseAccumulator latexState inputList =
    inputList
        |> List.foldl parserAccumulatorReducer ( [], latexState )


parserAccumulatorReducer : Reducer String ( List (List LatexExpression), LatexState )
parserAccumulatorReducer =
    parserReducerTransformer Parser.parse updateStateReducer


{-| parserReducerTransformer parse updateStateReducer is a Reducer input acc
-}
parserReducerTransformer : ParserReducerTransformer String (List LatexExpression) LatexState
parserReducerTransformer parse updateStateReducer input acc =
    let
        ( outputList, state ) =
            acc

        parsedInput =
            parse input

        newState =
            updateStateReducer parsedInput state
    in
    ( outputList ++ [ parsedInput ], newState )


type alias ParserReducer =
    Reducer (List LatexExpression) LatexState


renderAccumulatorReducer : Reducer (List LatexExpression) ( List String, LatexState )
renderAccumulatorReducer =
    renderTransformer renderLatexList updateStateReducer


renderAccumulator :
    LatexState
    -> List (List LatexExpression)
    -> ( List String, LatexState )
renderAccumulator latexState inputList =
    inputList
        |> List.foldl renderAccumulatorReducer ( [], latexState )


{-| renderTransformer render updateStateReducer is a Reducer input acc
-}
renderTransformer : RenderReducerTransformer LatexState (List LatexExpression) String
renderTransformer render updateStateReducer input acc =
    let
        ( outputList, state ) =
            acc

        newState =
            updateStateReducer input state

        renderedInput =
            render newState input
    in
    ( outputList ++ [ renderedInput ], newState )


info : LatexExpression -> LatexInfo
info latexExpression =
    case latexExpression of
        Macro name args ->
            { typ = "macro", name = name, value = args }

        Environment name body ->
            { typ = "env", name = name, value = [ body ] }

        _ ->
            { typ = "null", name = "null", value = [] }



{- UPDATERS -}


updateStateReducer : Reducer (List LatexExpression) LatexState
updateStateReducer parsedParagraph latexState =
    let
        headElement =
            parsedParagraph
                |> List.head
                |> Maybe.map info
                |> Maybe.withDefault (LatexInfo "null" "null" [ Macro "null" [] ])

        he =
            { typ = "macro", name = "setcounter", value = [ LatexList [ LXString "section" ], LatexList [ LXString "7" ] ] }

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
                    let
                        label =
                            getCounter "s1" latexState |> (\x -> x + 1) |> toString
                    in
                    latexState
                        |> incrementCounter "s1"
                        |> updateCounter "s2" 0
                        |> updateCounter "s3" 0
                        |> addSection (PT.unpackString headElement.value) label 1

                ( "macro", "subsection" ) ->
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

                ( "macro", "subsubsection" ) ->
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
                        |> addSection (PT.unpackString headElement.value) label 3

                ( "macro", "title" ) ->
                    setDictionaryItemForMacro "title" headElement latexState

                ( "macro", "author" ) ->
                    setDictionaryItemForMacro "author" headElement latexState

                ( "macro", "date" ) ->
                    setDictionaryItemForMacro "date" headElement latexState

                ( "macro", "email" ) ->
                    setDictionaryItemForMacro "email" headElement latexState

                ( "macro", "revision" ) ->
                    setDictionaryItemForMacro "revision" headElement latexState

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


setDictionaryItemForMacro macroname headElement latexState =
    let
        value =
            PT.unpackString headElement.value
    in
    setDictionaryItem macroname value latexState


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
            |> String.Extra.replace "\\section{" ("\\section{" ++ toString (s1 + 1) ++ " ")
    else if String.contains "\\subsection" paragraph then
        paragraph
            |> String.Extra.replace "\\subsection{" ("\\subsection{" ++ toString s1 ++ "." ++ toString (s2 + 1) ++ " ")
    else if String.contains "\\subsubsection" paragraph then
        paragraph
            |> String.Extra.replace "\\subsubsection{" ("\\subsubsection{" ++ toString s1 ++ "." ++ toString s2 ++ "." ++ toString (s3 + 1) ++ " ")
    else
        paragraph



{- HANDLERS -}


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



{- HELPERS -}


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
