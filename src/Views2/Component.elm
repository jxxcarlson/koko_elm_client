module Views2.Component exposing (..)

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
import External
import Utility


navigation : Model -> Element Styles variation Msg
navigation model =
    row NavBar
        [ justify, paddingXY 30 4 ]
        [ el Logo [ alignBottom, padding 8 ] (text "Noteshare")
        , searchForm model
        , menu model
        , pageSelector model
        , loginButton model
        ]


searchForm model =
    row NavBar
        [ spacing 15 ]
        [ inputText SearchField
            [ EE.onInput SetSearchTerm
            , Utility.onKeyUp (DoSearch model.searchState.domain)
            , placeholder "Search"
            , height (px 29)
            ]
            (model.searchState.query)
        , el FlatButton
            [ EA.width (px 40)
            , EA.center
            , EE.onClick (DoSearch Private 13)
            , EA.height (px 30)
            , padding 8
            ]
            (text "S1")
        , el FlatButton
            [ EA.width (px 40)
            , EA.center
            , EE.onClick (DoSearch Public 13)
            , EA.height (px 30)
            , padding 8
            ]
            (text "S2")
        ]


loginButton model =
    el Button
        [ EA.width (px 85)
        , EA.center
        , EE.onClick AuthenticationAction
        , EA.height (px 30)
        , padding 8
        ]
        (EL.text (authenticationButtonText model))


authenticationButtonText : Model -> String
authenticationButtonText model =
    if model.appState.signedIn then
        "Sign out"
    else
        "Sign in"


menu1 model =
    el FlatButton [ EA.width (px 100), EA.height (px 30), padding 8, EE.onClick ToggleMenu ] (EL.text "Tools")
        |> below
            [ when model.appState.menuDropped <|
                column Menu
                    [ padding 8, spacing 8 ]
                    [ el FlatButton [ EA.width (px 85), EE.onClick ToggleMenu ] (paragraph None [ EA.height (px 30), padding 8 ] [ EL.text "AAAA" ])
                    , el FlatButton [ EA.width (px 85), EE.onClick ToggleMenu ] (paragraph None [ EA.height (px 30), padding 8 ] [ EL.text "BBBB" ])
                    , el FlatButton [ EA.width (px 85), EE.onClick ToggleMenu ] (paragraph None [ EA.height (px 30), padding 8 ] [ EL.text "CCCC" ])
                    ]
            ]


menu model =
    el FlatButton [ EA.width (px 100), EA.height (px 30), padding 8, EE.onClick ToggleMenu ] (EL.text "Tools")
        |> below
            [ when model.appState.menuDropped <|
                column Menu
                    [ padding 8, spacing 8 ]
                    [ el FlatButton [ EA.width (px 85), EE.onClick ToggleMenu, EA.height (px 30), padding 8 ] (EL.text "AAAA")
                    , el FlatButton [ EA.width (px 85), EE.onClick ToggleMenu, EA.height (px 30), padding 8 ] (EL.text "BBBB")
                    , el FlatButton [ EA.width (px 85), EE.onClick ToggleMenu, EA.height (px 30), padding 8 ] (EL.text "CCCC")
                    ]
            ]


toolSelectorPanel model =
    row Panel
        [ paddingXY 10 6, spacing 15, center ]
        [ el FlatButton [ EA.width (px 85), EE.onClick (SelectTool TableOfContents), EA.height (px 30), padding 8 ] (EL.text "TOC")
        , el FlatButton [ EA.width (px 85), EE.onClick (SelectTool EditorTools), EA.height (px 30), padding 8 ] (EL.text "Tools")
        ]



-- External.askToReconnectUser "reconnectUser"
-- External.askToReconnectUser "reconnectUser"


editorPanel model =
    row Panel
        [ paddingXY 10 6, spacing 15, center ]
        [ el FlatButton [ EA.width (px 85), EE.onClick (NewDocument), EA.height (px 30), padding 8 ] (EL.text "New")
        , el FlatButton [ EA.width (px 85), EE.onClick (Refresh), EA.height (px 30), padding 8 ] (EL.text "Refresh")
        , full PanelInfo [ padding 11 ] (EL.text ("Words: " ++ (toString <| wordCount <| model.current_document)))
        ]



-- [ el FlatButton [ EA.width (px 85), EE.onClick (EE.onClick NewDocument), EA.height (px 30), padding 8 ] (EL.text "New document")
-- , text ("Words: " ++ (toString <| wordCount <| model.current_document))
-- ]
-- div [ id "footer" ]
--     [ span [ id "message" ] [ text model.message ]
--     , span [ id "info" ] [ text model.info ]
--     ]


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
        , el FooterNote [ alignBottom, padding 8 ] (text model.info)
        ]
    )
