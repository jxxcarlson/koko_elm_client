module LatexParser.Accumulator exposing (getSectionNumber, transformText, transformParagraphs, accumulator, processParagraph, processParagraph2, sectionCounter)

import LatexParser.Render as Render
import LatexParser.Parser
import String.Extra
import Document.Differ as Differ
import LatexParser.Render as Render
import LatexParser.Parser
import Regex


type alias LatexState =
    { s1 : Int, s2 : Int, s3 : Int }


transformText : String -> List String
transformText text =
    text
        |> Differ.paragraphify
        |> accumulator processParagraph2 sectionCounter
        |> Tuple.first


transformParagraphs : List String -> List String
transformParagraphs paragraphs =
    paragraphs
        |> accumulator processParagraph2 sectionCounter
        |> Tuple.first



-- > List.map (LatexParser.Parser.latexListGet >> List.map Render.transformLatex)
-- |> latexListGet
-- |> List.map transformLatex
-- |> List.map (List.map Render.transformLatex)
-- |> String.join ("")
-- |> (\x -> "\n<p>\n" ++ x ++ "\n</p>\n")


accumulator : (a -> LatexState -> b) -> (a -> LatexState -> LatexState) -> List a -> ( List b, LatexState )
accumulator processor stateUpdater inputList =
    inputList
        |> List.foldl (transformer processor stateUpdater) ( [], { s1 = 0, s2 = 0, s3 = 0 } )


transformer f g x acc =
    let
        ( a, b ) =
            acc
    in
        ( a ++ [ f x b ], g x b )


sectionCounter : String -> LatexState -> LatexState
sectionCounter paragraph latexState_ =
    let
        initialSectionNumber =
            (getSectionNumber paragraph)

        latexState =
            if initialSectionNumber > -1 then
                { latexState_ | s1 = (initialSectionNumber - 1), s2 = 0, s3 = 0 }
            else
                latexState_
    in
        if String.contains "\\section" paragraph then
            { latexState | s1 = latexState.s1 + 1, s2 = 0, s3 = 0 }
        else if String.contains "\\subsection" paragraph then
            { latexState | s2 = latexState.s2 + 1, s3 = 0 }
        else if String.contains "\\subsubsection" paragraph then
            { latexState | s3 = latexState.s3 + 1 }
        else
            latexState


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
