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

onChange : msg -> Attribute variation msg
onChange message =
    on "change" (Json.succeed message)

searchOptionsMenu model =
  -- select "searchMode" TOC [ width (px 120), EA.verticalCenter, on "change" (Json.map SelectSearchMode Json.string)]
  select "searchMode" TOC [ width (px 120), EA.verticalCenter, onInput SelectSearchMode]
      [ option "public" (model.searchState.domain == Public) (text "Public docs")
      , option "private" (model.searchState.domain == Private) (text "My docs")
      , option "all" (model.searchState.domain == All) (text "All docs")
      ]

searchForm : Model -> Element Styles variation Msg
searchForm model =
    row NavBar
        [ spacing 10, verticalCenter ]
        [ row Zero [ spacing -30] [
          inputText SearchField
            [ EE.onInput SetSearchTerm
            , Utility.onKeyUp (DoSearch model.searchState.domain)
            , placeholder "Search"
            , height (px 29), width (px 300),
            paddingXY -20 0
            ]
            (model.searchState.query)
         , circle 10 ClearButton [verticalCenter, paddingXY 6.5 9.0, onClick ClearSearch] (text "x")
        ]
        , row Zero
            [ center, spacing 15, paddingXY 10 0]
            [ searchButton model
            , Utility.visibleIf model.appState.signedIn (searchOptionsMenu model)
            ]
        ]


searchButton : Model -> Element Styles variation Msg
searchButton model =
    el Zero
        [ EA.width (px 25)
        , title "Search for my documents"
        , EA.alignRight
        , EE.onClick (DoSearch model.searchState.domain 13)
        , EA.height (px 30)
        , paddingXY 0 4
        ]
        (searchIcon model Private)


searchIcon : Model -> SearchDomain -> Element style variation msg
searchIcon model searchDomain =
  (EL.html (FontAwesome.search (Color.white) 25))


loginButton : style -> Model -> Element style variation Msg
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
                    , setTextTypeButton "latex" "Latex" model
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
        [ el (activeButton HomePage model) [ EE.onClick (GoTo HomePage), alignBottom, height (px 30), padding 8 ] (text "Start")
        , el (activeButton ReaderPage model) [ EE.onClick (GoTo ReaderPage), alignBottom, height (px 30), padding 8 ] (text "Reader")
        , el (activeButton EditorPage model) [ EE.onClick (GoTo EditorPage), alignBottom, height (px 30), padding 8 ] (text "Editor")
        , el (activeButton ImagePage model) [ EE.onClick (GoTo ImagePage), alignBottom, height (px 30), padding 8 ] (text "Image")
        , Utility.visibleIf False (el (activeButton AdminPage model) [ EE.onClick (GoTo AdminPage), alignBottom, height (px 30), padding 8 ] (text "Admin"))
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
        [ justify, paddingXY 30 4, alignBottom, width (percent 100) ]
        [ el FooterNote [ alignBottom, padding 8 ] (text model.message)
        ,  (onlineStatusIndicator model)
        ]
    )

hostString =
  Api.host |> String.split("//") |> List.Extra.last |> Maybe.withDefault ""

onlineStatusIndicator : Model -> Element Styles variation msg
onlineStatusIndicator model =
  el (onlineStatusStyle model) [ alignBottom, padding 8 ] (text ((onlineStatus model) ++ " at " ++ hostString ))

onlineStatus : Model -> String
onlineStatus model =
    if model.appState.online then
        "Online"
    else
        "Offline"


onlineStatusStyle : Model -> Styles
onlineStatusStyle model =
    if model.appState.online then
        StatusSuccess
    else
        StatusFailure
