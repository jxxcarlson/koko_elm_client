module LatexParser.Paragraph exposing (..)

import Char
import Parser exposing (..)
import LatexParser.Render as Render exposing (transformText)


{-
   > import LatexParser.Paragraph as P
   > P.parseDocument "abc def\nghi jxl\n\nmno pqr"
   Ok ([["abc def","ghi jxl"],["mno pqr"]])
       : Result.Result Parser.Error (List (List String))
-}


formatParagraph1 : List String -> String
formatParagraph1 lineList =
    "<p>\n" ++ (String.join "\n" lineList) ++ "\n</p>"


formatParagraph : List String -> String
formatParagraph lineList =
    let
        paragraph =
            (String.join "\n" lineList)
                ++ "\n\n"
    in
        "<p>\n" ++ (Render.transformText paragraph) ++ "\n</p>"


formatParagraphList : List (List String) -> String
formatParagraphList paragraphList =
    String.join "\n\n" (List.map formatParagraph paragraphList)


formatDocument : String -> String
formatDocument text =
    let
        paragraphList =
            parseDocument text
    in
        case paragraphList of
            Ok paragraphList ->
                formatParagraphList paragraphList

            err ->
                "There is a syntax error in your document."


parseDocument : String -> Result Error (List (List String))
parseDocument text =
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
