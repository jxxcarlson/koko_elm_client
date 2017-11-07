module MiniLatex.LatexDiffer exposing (..)

import MiniLatex.Accumulator as Accumulator
import MiniLatex.Differ as Differ exposing (EditRecord)
import MiniLatex.LatexState exposing (LatexState, emptyLatexState)
import MiniLatex.Render as Render
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


initialize2 : LatexState -> String -> EditRecord
initialize2 transformParagraphs text =
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
