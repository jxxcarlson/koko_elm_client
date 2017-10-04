module Document.LatexDiffer exposing (..)

import Document.Dictionary as Dictionary
import Document.Differ as Differ exposing (EditRecord)
import Document.Preprocess as Preprocess
import LatexParser.Render as Render
import String.Extra
import Regex
import Types exposing (DocumentDict)


initialize : DocumentDict -> String -> EditRecord
initialize documentDict text =
    text
        |> prepareContentForLatex
        |> Differ.initialize (Render.transformText >> (prependMacros documentDict))


update : DocumentDict -> EditRecord -> String -> EditRecord
update documentDict editorRecord text =
    text
        |> prepareContentForLatex
        |> Differ.update (Render.transformText >> (prependMacros documentDict)) editorRecord


safeUpdate documentDict editRecord content =
    if Differ.isEmpty editRecord then
        initialize documentDict content
    else
        update documentDict editRecord content


{-| replaceStrings is used by the document prepreprocessor
to normalize input to parseDocument.
-}
replaceStrings : String -> String
replaceStrings text =
    text
        |> String.Extra.replace "---" "\\ndash{}"
        |> String.Extra.replace "--" "\\mdash{}"


macros : DocumentDict -> String
macros documentDict =
    if (Dictionary.member "texmacros" documentDict) then
        Dictionary.getContent "texmacros" documentDict
            |> Regex.replace Regex.All (Regex.regex "\n+") (\_ -> "\n")
            |> String.Extra.replace "$$" "\n$$\n"
    else
        ""


prependMacros : DocumentDict -> String -> String
prependMacros documentDict content =
    let
        macroDefinitions =
            macros documentDict
    in
        macroDefinitions ++ "\n\n" ++ content


prepareContentForLatex : String -> String
prepareContentForLatex content =
    content
        |> replaceStrings



--            |> (\x -> (macros ++ "\n\n\n\n" ++ x))
-- updateEditRecord documentDict editRecord content =
--     let
--         preparedContent =
--             prepareContentForLatex content documentDict
--     in
--         if Differ.isEmpty editRecord then
--             initialize documentDict preparedContent
--         else
--             update documentDict editRecord preparedContent
