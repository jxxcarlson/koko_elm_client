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
import External
import Utility
import FontAwesome
import StyleSheet exposing (..)


navigation : Model -> Element Styles variation Msg
navigation model =
    row NavBar
        [ justify, paddingXY 30 4 ]
        [ el Logo [ alignBottom, padding 8 ] (text "Noteshare")
        , searchForm model
        , menu model
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
