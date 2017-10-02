module Document.LatexDiffer exposing (..)

import Document.Dictionary as Dictionary
import Document.Differ as Differ exposing (EditRecord)
import Document.Preprocess as Preprocess
import LatexParser.Render as Render
import String.Extra
import Regex
import Types exposing (DocumentDict)


initialize : String -> EditRecord
initialize text =
    Differ.initialize Render.transformText text


update : String -> EditRecord -> EditRecord
update text editorRecord =
    Differ.update Render.transformText text editorRecord


safeUpdate content editRecord =
    if Differ.isEmpty editRecord then
        initialize content
    else
        update content editRecord


{-| replaceStrings is used by the document prepreprocessor
to normalize input to parseDocument.
-}
replaceStrings : String -> String
replaceStrings text =
    text
        |> String.Extra.replace "--" "–"
        |> String.Extra.replace "---" "—"


prepareContentForLatex : String -> DocumentDict -> String
prepareContentForLatex content documentDict =
    let
        macros =
            if (Dictionary.member "texmacros" documentDict) then
                Dictionary.getContent "texmacros" documentDict
                    |> Regex.replace Regex.All (Regex.regex "\n+") (\_ -> "\n")
                    |> String.Extra.replace "$$" "\n$$\n"
            else
                ""
    in
        content
            |> replaceStrings
            |> Preprocess.transformXLinks
            |> (\x -> (macros ++ "\n\n" ++ x))


updateEditRecord content documentDict editRecord =
    let
        preparedContent =
            prepareContentForLatex content documentDict
    in
        if Differ.isEmpty editRecord then
            initialize content
        else
            update content editRecord
