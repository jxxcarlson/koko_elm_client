module Document.MiniLatexDiffer exposing (..)

import MiniLatex.Accumulator as Accumulator
import Document.Dictionary as Dictionary
import Document.Differ as Differ exposing (EditRecord)
import Document.Preprocess as Preprocess
import MiniLatex.Render as Render
import String.Extra
import Regex
import Types exposing (DocumentDict)


initialize : String -> EditRecord
initialize text =
    text
        |> prepareContentForLatex
        |> Differ.initialize Render.transformText


initialize2 : String -> EditRecord
initialize2 text =
    text
        |> prepareContentForLatex
        |> Differ.initialize2 Accumulator.transformParagraphs


update : EditRecord -> String -> EditRecord
update editorRecord text =
    text
        |> prepareContentForLatex
        |> Differ.update Render.transformText editorRecord


safeUpdate editRecord content =
    if Differ.isEmpty editRecord then
        initialize2 content
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
