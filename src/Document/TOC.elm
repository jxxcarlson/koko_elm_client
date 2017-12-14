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
        section =
            if level == 1 then
                previousLabel.section + 1
            else
                previousLabel.section

        subsection =
            if level == 2 then
                previousLabel.subsection + 1
            else
                0
    in
    { section = section, subsection = subsection }


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
