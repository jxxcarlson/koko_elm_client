module Document.TOC exposing (..)

import Types exposing (Document, DocumentAttributes)
import Utility.KeyValue as KeyValue


{- TOC labels -}


type alias TOCLabel =
    { section : Int, subsection : Int }


tocLabel : Document -> String
tocLabel document =
    let
        maybeSectionNumber =
            KeyValue.getIntValueForKeyFromTagList "sectionNumber" document.tags

        maybeSubSectionNumber =
            KeyValue.getIntValueForKeyFromTagList "subsectionNumber" document.tags

        sectionNumber =
            case maybeSectionNumber of
                Just k ->
                    k

                Nothing ->
                    0

        subsectionNumber =
            case maybeSubSectionNumber of
                Just k ->
                    k

                Nothing ->
                    0
    in
    TOCLabel sectionNumber subsectionNumber |> tocLabelText


{-| currentLabel level previousLabel computes the next TOC label as
a function of the current level and previous label
-}
currentLabel : Int -> TOCLabel -> TOCLabel
currentLabel level previousLabel =
    let
        realLevel =
            level - 1

        section =
            if realLevel == 1 then
                previousLabel.section + 1
            else
                previousLabel.section

        subsection =
            if realLevel == 2 then
                previousLabel.subsection + 1
            else
                0
    in
    { section = section, subsection = subsection }


makeLabel : Document -> List TOCLabel -> TOCLabel
makeLabel document labelList =
    case List.head labelList of
        Just previousLabel ->
            currentLabel document.attributes.level previousLabel

        Nothing ->
            TOCLabel 0 0


tocLabelsForDocumentList : List Document -> List TOCLabel
tocLabelsForDocumentList documentList =
    documentList
        |> List.foldr (\doc labels -> [ makeLabel doc labels ] ++ labels) [ TOCLabel 0 0 ]
        |> List.drop 1
        |> List.reverse


updateSectionNumberTags : TOCLabel -> Document -> Document
updateSectionNumberTags tocLabel document =
    let
        tags =
            document.tags
                |> KeyValue.setIntValueForKeyInTagList "sectionNumber" tocLabel.section
                |> KeyValue.setIntValueForKeyInTagList "subsadsectionNumber" tocLabel.subsection
    in
    { document | tags = tags }


setDocumentLevels : List Document -> List Document
setDocumentLevels documentList =
    let
        labelList =
            tocLabelsForDocumentList documentList
    in
    List.map2 updateSectionNumberTags labelList documentList


tocLabelText : TOCLabel -> String
tocLabelText label =
    let
        sectionLabel =
            if label.section > 0 then
                toString label.section
            else
                ""

        fullLabel =
            if label.subsection > 0 then
                sectionLabel ++ "." ++ toString label.subsection
            else
                sectionLabel
    in
    fullLabel


setCounterTextForLabel : TOCLabel -> String
setCounterTextForLabel label =
    let
        sectionText =
            if label.section > 0 then
                "\\setcounter{section}{" ++ toString label.section ++ "}"
            else
                ""

        subsectionText =
            if label.subsection > 0 then
                "\\setcounter{subsection}{" ++ toString label.subsection ++ "}"
            else
                ""
    in
    String.join "\n" [ sectionText, subsectionText ]
