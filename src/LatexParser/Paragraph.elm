module LatexParser.Paragraph exposing (formatDocument, parseDocument, replaceStrings)

import Char
import Parser exposing (..)
import LatexParser.Render as Render exposing (transformText)
import String.Extra


{-| PARSER: The function `parseDocument` takes a documument
(string) as input and returns a `List (List String)`,
where a `(List String)` represents a paragraph.
-}
parseDocument : String -> Result Error (List (List String))
parseDocument text =
    let
        _ =
            Debug.log "parseDocument" "now"
    in
        Parser.run document (text ++ "\n\n")


document : Parser (List (List String))
document =
    inContext "document" <|
        succeed identity
            |= repeat zeroOrMore paragraph


paragraph : Parser (List String)
paragraph =
    inContext "paragraph" <|
        succeed identity
            |. ws
            |= repeat zeroOrMore line
            |. symbol "\n"
            |. ws


line : Parser String
line =
    inContext "line" <|
        succeed identity
            |= keep oneOrMore (\c -> c /= '\n')
            |. symbol "\n"


ws : Parser ()
ws =
    ignore zeroOrMore (\c -> c == ' ' || c == '\n')



{- Examples:

    > import LatexParser.Paragraph as P
    > P.parseDocument "abc def\nghi jxl\n\nmno pqr"
    Ok ([["abc def","ghi jxl"],["mno pqr"]])
        : Result.Result Parser.Error (List (List String))

   formatParagraph1 : List String -> String
   formatParagraph1 lineList =
       "<p>\n" ++ (String.join "\n" lineList) ++ "\n</p>"
-}


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
            parseDocument text
    in
        case paragraphList of
            Ok paragraphList ->
                formatParagraphList paragraphList

            err ->
                "There is a syntax error in your document."


formatParagraphList : List (List String) -> String
formatParagraphList paragraphList =
    let
        _ =
            Debug.log "formatParagraphList" "now"
    in
        String.join "\n\n" (List.map formatParagraph paragraphList)


formatParagraph : List String -> String
formatParagraph lineList =
    let
        _ =
            Debug.log "formatParagraph" "now"

        paragraph =
            (String.join "\n" lineList)
                ++ "\n\n"
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
