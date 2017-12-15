module Views.NavBar exposing (loginButton, navigation)

import Action.UI as UI
import Color
import Configuration
import Element as EL exposing (..)
import Element.Attributes as EA exposing (..)
import Element.Events as EE exposing (..)
import Element.Keyed as Keyed
import FontAwesome
import Json.Decode as Json
import StyleSheet exposing (..)
import Types exposing (..)
import Utility
import Views.Basic as Basic
import Views.Component as Component
import Views.Utility as Utility


navigation : Model -> Element Styles variation Msg
navigation model =
    case model.device of
        Phone ->
            phoneNavigation model

        _ ->
            standardNavigation model


standardNavigation : Model -> Element Styles variation Msg
standardNavigation model =
    row NavBar
        [ justify, paddingXY 10 4 ]
        [ searchForm model

        --, menu model
        , pageSelector model
        , modeSelector model
        , loginButton FlatButtonBlue model
        ]


phoneNavigation : Model -> Element Styles variation Msg
phoneNavigation model =
    row NavBar
        [ paddingXY 8 4, spacing 0 ]
        [ basicSearchForm model
        , goMenu model
        ]


goMenu model =
    row NavBar
        [ spacing 6, paddingRight 8, moveLeft 10 ]
        [ --   (startPageIcon model)
          Basic.button "List" Charcoal [ width (px 40), onClick Types.InitStartPage ]
        , Basic.button "Read" Charcoal [ EE.onClick (GoTo ReaderPage), width (px 55), center ]
        , Basic.button (signInOutText model) Button [ onClick SignOutOrIn, width (px 60) ]
        ]


signInOutText : Model -> String
signInOutText model =
    if model.appState.signedIn then
        "Log out"
    else
        "Log in"


onChange : msg -> Attribute variation msg
onChange message =
    on "change" (Json.succeed message)


searchOptionsMenu : Model -> Element Styles variation Msg
searchOptionsMenu model =
    -- select "searchMode" TOC [ width (px 120), EA.verticalCenter, on "change" (Json.map SelectSearchMode Json.string)]
    select "searchMode"
        LightGray
        [ height (px 25), EA.verticalCenter, onInput SelectSearchMode ]
        [ option "public" (model.searchState.domain == Public) (text "Public")
        , option "private" (model.searchState.domain == Private) (text "My docs")
        , option "all" (model.searchState.domain == All) (text "All")
        ]


searchForm : Model -> Element Styles variation Msg
searchForm model =
    row NavBar
        [ spacing 8, verticalCenter ]
        [ row Zero
            []
            [ inputText SearchField
                [ EE.onInput UpdateSearchQueryInputBuffer
                , Utility.onKeyUp (DoSearch model.searchState.domain)
                , placeholder "Search: title, k:keyword .."
                , height (px 29)
                , width (px 300)
                ]
                model.searchQueryInputBuffer
            , circle 10 ClearButton [ moveLeft 25, verticalCenter, paddingXY 6.5 9.0, onClick ClearSearch ] (text "x")
            ]
        , row Zero
            [ spacing 10, moveLeft 18 ]
            [ -- searchButton model
              randomDocumentIcon model
            , Utility.visibleIf model.appState.signedIn (searchOptionsMenu model)

            -- , Utility.visibleIf model.appState.signedIn (searchOrderMenu model)
            ]
        ]


basicSearchForm : Model -> Element Styles variation Msg
basicSearchForm model =
    row NavBar
        [ spacing 8, verticalCenter ]
        [ row Zero
            []
            [ inputText SearchField
                [ EE.onInput UpdateSearchQueryInputBuffer
                , Utility.onKeyUp (DoSearch model.searchState.domain)
                , placeholder "Search: title, k:keyword .."
                , height (px 29)
                , minWidth (px 180)
                ]
                model.searchQueryInputBuffer
            , circle 10 ClearButton [ moveLeft 25, verticalCenter, paddingXY 6.5 9.0, onClick ClearSearch ] (text "x")
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
    EL.html (FontAwesome.search Color.white 25)


loginButton : Styles -> Model -> Element Styles variation Msg
loginButton style model =
    Basic.button
        (authenticationButtonText model)
        FlatButtonBlue
        [ EE.onClick AuthenticationAction
        , EA.width (px 70)
        , EA.height (px 30)
        ]


pageSelector : Model -> Element Styles variation Msg
pageSelector model =
    row NavBar
        [ spacing 12 ]
        [ userHomePagesIcon model
        , Utility.visibleIf ((model.window.width > Configuration.tabletWidth) && model.appState.signedIn) (userPreferencesIcon model)
        , Utility.visibleIf model.appState.signedIn (homepageIcon model)

        -- , el NavBar [ alignBottom, height (px 30), padding 8 ] (startPageIcon model)
        , startPageIcon model
        ]


modeSelector : Model -> Element Styles variation Msg
modeSelector model =
    row NavBar
        [ spacing 8 ]
        [ Utility.visibleIf (model.appState.signedIn && (model.window.width > Configuration.tabletWidth)) (newDocumentButton model)
        , Basic.button "Reader" (Component.activeButton ReaderPage model) [ EE.onClick (GoTo ReaderPage), width (px 60), center ]
        , Utility.visibleIf (model.appState.signedIn && (model.window.width > Configuration.tabletWidth))
            (Basic.button "Editor" (Component.activeButton EditorPage model) [ EE.onClick (GoTo EditorPage), width (px 50), center ])

        -- , Utility.visibleIf model.appState.signedIn (el (activeButton ImagePage model) [ EE.onClick (GoTo ImagePage), alignBottom, height (px 30), padding 8 ] (text "Image"))
        , Utility.visibleIf ((model.current_user.username == "jxxcarlson") && (model.window.width > Configuration.tabletWidth))
            (Basic.button "Admin" (Component.activeButton AdminPage model) [ EE.onClick (GoTo AdminPage), width (px 55), center ])
        ]


authenticationButtonText : Model -> String
authenticationButtonText model =
    if model.appState.signedIn then
        "Sign out"
    else
        "Sign in"


homepageIcon : Model -> Element Styles variation Msg
homepageIcon model =
    Basic.faIcon "Home Page" FontAwesome.home [ onClick Types.UserHomePage ]


randomDocumentIcon : Model -> Element Styles variation Msg
randomDocumentIcon model =
    Basic.faIcon "Get random documents" FontAwesome.random [ onClick RandomDocuments ]


userHomePagesIcon : Model -> Element Styles variation Msg
userHomePagesIcon model =
    Basic.faIcon "User Pages" FontAwesome.group [ onClick Types.GotoUserHomePages ]



-- Basic.icon 3 20 "User Pages" Types.GotoUserHomePages FontAwesome.asterisk model
-- GotoUserHomePages


userPreferencesIcon : Model -> Element Styles variation Msg
userPreferencesIcon model =
    Basic.faIcon "User Preferences" FontAwesome.list_alt [ onClick Types.GotoUserPreferencesPage ]


startPageIcon : Model -> Element Styles variation Msg
startPageIcon model =
    Basic.faIcon "Start page" FontAwesome.asterisk [ onClick Types.InitStartPage ]


newDocumentButton : Model -> Element Styles variation Msg
newDocumentButton model =
    Basic.faIcon "New document" FontAwesome.plus [ onClick NewDocument ]
