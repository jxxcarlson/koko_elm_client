module Views.Component exposing (..)

import StyleSheet exposing (..)
import Color
import Element as EL exposing (..)
import Element.Attributes as EA exposing (..)
import Element.Events as EE exposing (..)
import Types exposing (..)
import Utility
import FontAwesome
import StyleSheet exposing (..)
import Request.Api as Api
import List.Extra
import Json.Decode as Json
import Views.Utility as Utility
import Views.Basic as Basic


cancelAuthentication style model =
  Basic.button
    "Cancel"
    Button [
    EE.onClick CancelAuthentication, EA.width (px 85), EA.height (px 30)]


-- https://ellie-app.com/3Gqxw7zLGzTa1/6


textFormatMenu model =
    el HeaderLabel [ EA.width (px 101), EA.height (px 30), paddingXY 8 14, EE.onClick (ToggleMenu "textType") ] (EL.text "Format")
        |> below
            [ --when model.appState.textTypeMenuDropped <|
                column Menu
                    [ padding 8, spacing 2 ]
                    [ setTextTypeButton "plain" "Plain" model
                    , setTextTypeButton "adoc" "Asciidoc" model
                    , setTextTypeButton "adoc_latex" "Ascii/Latex" model
                    -- , setTextTypeButton "latex" "Latex" model
                    ]
            ]


setTextTypeButton textType label model =
  el (textFormatButton textType model) [ EA.width (px 85), EE.onClick (SetTextType textType), EA.height (px 30), paddingXY 8 14 ] (EL.text label)


textFormatButton textFormat model =
    if textFormat == model.current_document.attributes.textType then
        ActiveFlatButton
    else
        FlatButton


docTypeMenu model =
    el HeaderLabel [ EA.width (px 101), EA.height (px 30), paddingXY 8 14, EE.onClick (ToggleMenu "docType") ] (EL.text "Type")
        |> below
            [ -- when model.appState.docTypeMenuDropped <|
                column Menu
                    [ padding 8, spacing 2 ]
                    [ el (docTypeButton "standard" model) [ EA.width (px 85), EE.onClick (SetDocType "standard"), EA.height (px 30), paddingXY 8 14 ] (EL.text "Standard")
                    -- , el (docTypeButton "note" model) [ EA.width (px 85), EE.onClick (SetDocType "note"), EA.height (px 30), paddingXY 8 14 ] (EL.text "Note")
                    , el (docTypeButton "master" model) [ EA.width (px 85), EE.onClick (SetDocType "master"), EA.height (px 30), paddingXY 8 14 ] (EL.text "Master")
                    ]
            ]


docTypeButton docType model =
    if docType == model.current_document.attributes.docType then
        ActiveFlatButton
    else
        FlatButton


toolSelectorColor model tool =
    if model.appState.tool == tool then
        Color.white
    else
        Color.gray



activeButton : Page -> Model -> Styles
activeButton currentPage model =
    if currentPage == model.appState.page then
        ActiveFlatButton
    else
        FlatButton
