module MiniLatex.LatexDiffer exposing (..)

import MiniLatex.Accumulator as Accumulator
import Document.Dictionary as Dictionary
import MiniLatex.Differ as Differ exposing (EditRecord)
import Document.Preprocess as Preprocess
import MiniLatex.LatexState exposing (LatexState, emptyLatexState)
import MiniLatex.Render as Render
import String.Extra
import Regex
import Types exposing (DocumentDict)


initialize : String -> EditRecord
initialize text =
    text
        |> prepareContentForLatex
        |> Differ.initialize Render.transformText


initialize2 : LatexState -> String -> EditRecord
initialize2 latexState text =
    text
        |> prepareContentForLatex
        |> Differ.initialize2 (Accumulator.transformParagraphs latexState)


update : EditRecord -> String -> EditRecord
update editorRecord text =
    text
        |> prepareContentForLatex
        |> Differ.update Render.transformText editorRecord


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
