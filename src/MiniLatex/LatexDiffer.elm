module MiniLatex.LatexDiffer exposing (..)

import MiniLatex.Accumulator as Accumulator
import MiniLatex.Differ as Differ exposing (EditRecord)
import MiniLatex.LatexState exposing (LatexState, emptyLatexState)
import MiniLatex.Render as Render exposing (render, renderLatexList)
import String.Extra
import Regex


initialize : String -> EditRecord
initialize text =
    text
        |> prepareContentForLatex
        |> Differ.initialize (Render.transformText emptyLatexState)


initialize1 : LatexState -> String -> EditRecord
initialize1 latexState text =
    text
        |> prepareContentForLatex
        |> Differ.initialize2 (Accumulator.transformParagraphs latexState)


initialize2O : LatexState -> String -> EditRecord
initialize2O latexState text =
    let
        editRecord1 =
            text
                |> prepareContentForLatex
                |> Differ.initialize2 (Accumulator.transformParagraphs emptyLatexState)

        latexState2 =
            { emptyLatexState | crossReferences = editRecord1.latexState.crossReferences }

        editRecord2 =
            text
                |> prepareContentForLatex
                |> Differ.initialize2 (Accumulator.transformParagraphs latexState2)
    in
        editRecord2


type alias EditRecord =
    { paragraphs : List String
    , renderedParagraphs : List String
    , latexState : LatexState
    }


initialize2 : LatexState -> String -> EditRecord
initialize2 latexState text =
    let
        paragraphs =
            text
                |> prepareContentForLatex
                |> Differ.paragraphify

        ( latexExpressionList, latexState1 ) =
            paragraphs
                |> Accumulator.parseParagraphs emptyLatexState

        latexState2 =
            { emptyLatexState | crossReferences = latexState1.crossReferences }

        ( renderedParagraphs, latexState3 ) =
            latexExpressionList
                |> Accumulator.renderParagraphs latexState2
    in
        EditRecord paragraphs renderedParagraphs latexState2


initialize2a : LatexState -> String -> EditRecord
initialize2a latexState text =
    let
        paragraphs =
            text
                |> prepareContentForLatex
                |> Differ.paragraphify

        ( latexExpressionList, latexState ) =
            paragraphs
                |> Accumulator.parseParagraphs emptyLatexState

        renderedParagraphs =
            latexExpressionList |> List.map (renderLatexList latexState)
    in
        EditRecord paragraphs renderedParagraphs latexState


update : EditRecord -> String -> EditRecord
update editorRecord text =
    text
        |> prepareContentForLatex
        |> Differ.update (Render.transformText editorRecord.latexState) editorRecord


safeUpdate : EditRecord -> String -> EditRecord
safeUpdate editRecord content =
    if Differ.isEmpty editRecord then
        initialize2 emptyLatexState content
    else
        update editRecord content


{-| replaceStrings is used by the document prepreprocessor
to normalize input to parseDocument.
-}
replaceStrings : String -> String
replaceStrings text =
    text
        |> String.Extra.replace "---" "\\mdash{}"
        |> String.Extra.replace "--" "\\ndash{}"


prepareContentForLatex : String -> String
prepareContentForLatex content =
    content
        |> replaceStrings
