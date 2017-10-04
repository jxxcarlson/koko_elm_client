module Document.Differ exposing (..)

import Regex


paragraphify : String -> List String
paragraphify text =
    Regex.split Regex.All (Regex.regex "\n\n+") text
        |> List.filter (\x -> String.length x /= 0)


commonInitialSegment : List String -> List String -> List String
commonInitialSegment x y =
    if x == [] then
        []
    else if y == [] then
        []
    else
        let
            a =
                List.take 1 x

            b =
                List.take 1 y
        in
            if a == b then
                a ++ commonInitialSegment (List.drop 1 x) (List.drop 1 y)
            else
                []


commonTerminalSegment : List String -> List String -> List String
commonTerminalSegment x y =
    commonInitialSegment (List.reverse x) (List.reverse y) |> List.reverse


dropLast : Int -> List String -> List String
dropLast k x =
    x |> List.reverse |> List.drop k |> List.reverse


takeLast : Int -> List String -> List String
takeLast k x =
    x |> List.reverse |> List.take k |> List.reverse


type alias DiffRecord =
    { commonInitialSegment : List String
    , commonTerminalSegment : List String
    , middleSegmentInSource : List String
    , middleSegmentInTarget : List String
    }


type alias EditRecord =
    { paragraphs : List String
    , renderedParagraphs : List String
    }


initialize : (String -> String) -> String -> String -> EditRecord
initialize transformer preamble text =
    let
        paragraphs =
            paragraphify text

        renderedParagraphs =
            List.map transformer paragraphs
    in
        EditRecord paragraphs ([ preamble ++ "\n\n\n" ] ++ renderedParagraphs)


clear : EditRecord
clear =
    EditRecord [] []


isEmpty : EditRecord -> Bool
isEmpty editRecord =
    editRecord.paragraphs == [] && editRecord.renderedParagraphs == []


update : (String -> String) -> String -> EditRecord -> String -> EditRecord
update transformer preamble editorRecord text =
    let
        newParagraphs =
            paragraphify text

        diffRecord =
            diff editorRecord.paragraphs newParagraphs

        newRenderedParagraphs =
            renderDiff transformer diffRecord editorRecord.renderedParagraphs
    in
        EditRecord newParagraphs ([ preamble ++ "\n\n\n" ] ++ newRenderedParagraphs)


diff : List String -> List String -> DiffRecord
diff u v =
    let
        a =
            commonInitialSegment u v

        b =
            commonTerminalSegment u v

        la =
            List.length a

        lb =
            List.length b

        x =
            u |> List.drop la |> dropLast lb

        y =
            v |> List.drop la |> dropLast lb
    in
        DiffRecord a b x y


renderList : (String -> String) -> List String -> List String
renderList transformer inputList =
    List.map transformer inputList


renderDiff : (String -> String) -> DiffRecord -> List String -> List String
renderDiff renderer diffRecord renderedStringList =
    let
        ii =
            List.length diffRecord.commonInitialSegment

        it =
            List.length diffRecord.commonTerminalSegment

        initialSegmentRendered =
            List.take ii renderedStringList

        terminalSegmentRendered =
            takeLast it renderedStringList

        middleSegmentRendered =
            (renderList renderer) diffRecord.middleSegmentInTarget
    in
        initialSegmentRendered ++ middleSegmentRendered ++ terminalSegmentRendered
