module Views.Component exposing (..)

import Style exposing (..)
import StyleSheet exposing (..)
import Color
import Html as Html
import Element as EL exposing (..)
import Element.Attributes as EA exposing (..)
import Element.Events as EE exposing (..)
import Style exposing (..)
import Style.Border as Border
import Style.Color as Color
import Style.Font as Font
import Style.Transition as Transition
import Types exposing (..)
import Html.Events as HE exposing (onClick)
import Json.Decode as Json exposing (int, list, string, float, Decoder)
import Action.Document exposing (wordCount)
import Utility
import FontAwesome
import StyleSheet exposing (..)


navigation : Model -> Element Styles variation Msg
navigation model =
    row NavBar
        [ justify, paddingXY 30 4 ]
        [ el Logo [ alignBottom, padding 8 ] (text "Noteshare")
        , searchForm model
        --, menu model
        , pageSelector model
        , loginButton Button model
        ]


searchForm model =
    row NavBar
        [ spacing 10, verticalCenter ]
        [ inputText SearchField
            [ EE.onInput SetSearchTerm
            , Utility.onKeyUp (DoSearch model.searchState.domain)
            , placeholder "Search"
            , height (px 29)
            ]
            (model.searchState.query)
        , row Zero
            [ center, spacing 5 ]
            [ el Zero
                [ EA.width (px 25)
                , title "Search for my documents"
                , EA.alignRight
                , EE.onClick (DoSearch Private 13)
                , EA.height (px 30)
                , paddingXY 0 4
                ]
                (searchIcon model Private)
            , el Zero
                [ EA.width (px 25)
                , title "Search for public documents"
                , EA.alignLeft
                , EE.onClick (DoSearch Public 13)
                , EA.height (px 30)
                , paddingXY 0 4
                ]
                (searchIcon model Public)
            ]
        ]


searchIcon : Model -> SearchDomain -> Element style variation msg
searchIcon model searchDomain =
    if model.searchState.domain == searchDomain then
        (EL.html (FontAwesome.search Color.white 25))
    else
        (EL.html (FontAwesome.search Color.grey 25))


loginButton style model =
    el style
        [ EA.width (px 85)
        , EE.onClick AuthenticationAction
        , EA.height (px 30)
        , padding 8
        ]
        (EL.text (authenticationButtonText model))

cancelAuthentication style model =
    el style
        [ EA.width (px 85)
        , EE.onClick CancelAuthentication
        , EA.height (px 30)
        , padding 8
        ]
        (EL.text "Cancel")


authenticationButtonText : Model -> String
authenticationButtonText model =
    if model.appState.signedIn then
        "Sign out"
    else
        "Sign in"




textFormatMenu model =
    el FlatButton [ EA.width (px 100), EA.height (px 30), paddingXY 8 14, EE.onClick (ToggleMenu "textType")] (EL.text "Format")
        |> below
            [ when model.appState.textTypeMenuDropped <|
                column Menu
                    [ padding 8, spacing 2 ]
                    [ el (textFormatButton "plain" model) [ EA.width (px 85), EE.onClick (SetTextType "plain"), EA.height (px 30), paddingXY 8 14 ] (EL.text "Plain")
                    , el (textFormatButton "adoc" model) [ EA.width (px 85), EE.onClick (SetTextType "adoc"), EA.height (px 30), paddingXY 8 14 ] (EL.text "Asciidoc")
                    , el (textFormatButton "latex" model) [ EA.width (px 85), EE.onClick (SetTextType "latex"), EA.height (px 30), paddingXY 8 14 ] (EL.text "LaTeX")
                    ]
            ]


textFormatButton textFormat model =
  if textFormat == model.current_document.attributes.textType then
    ActiveFlatButton
  else
    FlatButton


docTypeMenu model =
    el FlatButton [ EA.width (px 100), EA.height (px 30), paddingXY 8 14, EE.onClick (ToggleMenu "docType")] (EL.text "Type")
        |> below
            [ when model.appState.docTypeMenuDropped <|
                column Menu
                    [ padding 8, spacing 2 ]
                    [ el (docTypeButton "standard" model) [ EA.width (px 85), EE.onClick (SetDocType "standard"), EA.height (px 30), paddingXY 8 14 ] (EL.text "Standard")
                    , el (docTypeButton "note" model) [ EA.width (px 85), EE.onClick (SetDocType "note"), EA.height (px 30), paddingXY 8 14 ] (EL.text "Note")
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



-- External.askToReconnectUser "reconnectUser"
-- External.askToReconnectUser "reconnectUser"


pageSelector : Model -> Element Styles variation Msg
pageSelector model =
    row NavBar
        [ spacing 8 ]
        [ el (activeButton HomePage model) [ EE.onClick (GoTo HomePage), alignBottom, height (px 30), padding 8 ] (text "Home")
        , el (activeButton ReaderPage model) [ EE.onClick (GoTo ReaderPage), alignBottom, height (px 30), padding 8 ] (text "Reader")
        , el (activeButton EditorPage model) [ EE.onClick (GoTo EditorPage), alignBottom, height (px 30), padding 8 ] (text "Editor")
        ]


activeButton : Page -> Model -> Styles
activeButton currentPage model =
    if currentPage == model.appState.page then
        ActiveFlatButton
    else
        FlatButton


footer : Model -> Element Styles variation msg
footer model =
    (row Footer
        [ justify, paddingXY 30 4 ]
        [ el FooterNote [ alignBottom, padding 8 ] (text model.message)
        , el (onlineStatusStyle model) [ alignBottom, padding 8 ] (text (onlineStatus model))
        ]
    )


onlineStatus : Model -> String
onlineStatus model =
    if model.appState.online then
        "Online"
    else
        "Offline"


onlineStatusStyle model =
    if model.appState.online then
        StatusSuccess
    else
        StatusFailure
