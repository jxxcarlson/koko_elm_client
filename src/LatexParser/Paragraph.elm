module LatexParser.Paragraph
    exposing
        ( formatDocument
        , paragraphify
        , replaceStrings
        , formatParagraphList
        )

import Char
import Parser exposing (..)
import LatexParser.Render as Render exposing (transformText)
import Regex
import String.Extra


{-| formatDocment takes a string as input and then

    1. parses it into paragraphs,
    2. Applies the MiniLaTeX parser to the paragraphs (as strings),
       and renders the result as HTML.
    3. The rendered paragaphs are then concatenated into a long string.

    NOTE: In step (2), it is the function call `LatexParser.Render.transformText paragraph`
    in  `LatexParser.Paragraph.formatParagraph` that drives parsing and
    rendering of LaTeX.  `Render.transformText` in turn calls on `Parser.run latexList text`
    which parses a paragraph into a list of LaTeX elements.  This list is piped into
    `List.map transformLatex`, which dispatches each Latex element to handler which
    converts it HTML.  The result, a list of HTML strings, is then concatenated to
    form an HTML string.

-}
formatDocument : String -> String
formatDocument text =
    let
        _ =
            Debug.log "formatDocument" "now"

        paragraphList =
            paragraphify text
    in
        formatParagraphList paragraphList


paragraphify : String -> List String
paragraphify text =
    Regex.split Regex.All (Regex.regex "\n\n+") text
        |> List.filter (\x -> String.length x /= 0)


formatParagraphList : List String -> String
formatParagraphList paragraphList =
    String.join "\n\n" (List.map formatParagraph paragraphList)


formatParagraph : String -> String
formatParagraph paragraph =
    let
        _ =
            Debug.log "formatParagraph" "now"
    in
        "<p>\n" ++ (Render.transformText paragraph) ++ "\n</p>"


{-| replaceStrings is used by the document prepreprocessor
to normalize input to parseDocument.
-}
replaceStrings : String -> String
replaceStrings text =
    text
        |> String.Extra.replace "\\]" "$$"
        |> String.Extra.replace "\\[" "$$"
        |> String.Extra.replace "--" "–"
        |> String.Extra.replace "---" "—"
