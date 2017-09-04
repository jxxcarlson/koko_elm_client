module Views.NavBar exposing(navigation, loginButton)

import StyleSheet exposing (..)
import Color
import Element as EL exposing (..)
import Element.Attributes as EA exposing (..)
import Element.Events as EE exposing (..)
import Types exposing (..)
import Utility
import FontAwesome
import StyleSheet exposing (..)
import Json.Decode as Json
import Views.Utility as Utility
import Views.Basic as Basic
import Views.Component as Component

navigation : Model -> Element Styles variation Msg
navigation model =
    row NavBar
        [ justify, paddingXY 10 4 ]
        [ searchForm model
          --, menu model
        , pageSelector model
        , loginButton Button model
        ]

onChange : msg -> Attribute variation msg
onChange message =
    on "change" (Json.succeed message)

searchOptionsMenu : Model -> Element Styles variation Msg
searchOptionsMenu model =
  -- select "searchMode" TOC [ width (px 120), EA.verticalCenter, on "change" (Json.map SelectSearchMode Json.string)]
  select "searchMode" LightGray [ height (px 25), EA.verticalCenter, onInput SelectSearchMode]
      [ option "public" (model.searchState.domain == Public) (text "Public")
      , option "private" (model.searchState.domain == Private) (text "My docs")
      , option "all" (model.searchState.domain == All) (text "All")
      ]

searchForm : Model -> Element Styles variation Msg
searchForm model =
    row NavBar
        [ spacing 5, verticalCenter ]
        [ row Zero [] [
          inputText SearchField
            [ EE.onInput UpdateSearchQueryInputBuffer
            , Utility.onKeyUp (DoSearch model.searchState.domain)
            , placeholder "Search: title, k:keyword .."
            , height (px 29), minWidth (px 180)
            ]
            (model.searchQueryInputBuffer)
         , circle 10 ClearButton [moveLeft 25, verticalCenter, paddingXY 6.5 9.0, onClick ClearSearch] (text "x")
        ]
        , row Zero
            [spacing 10, moveLeft 18]
            [
            -- searchButton model
             randomDocumentIcon model
            , Utility.visibleIf model.appState.signedIn (searchOptionsMenu model)
            -- , Utility.visibleIf model.appState.signedIn (searchOrderMenu model)
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


loginButton : Styles -> Model -> Element Styles variation Msg
loginButton style model =
   Basic.button
     (authenticationButtonText model)
     Button [
     EE.onClick AuthenticationAction, EA.width (px 70), EA.height (px 30)]



pageSelector : Model -> Element Styles variation Msg
pageSelector model =
    row NavBar
        [ spacing 12 ]
        [ (userHomePagesIcon model)
        , (userPreferencesIcon model)
        , Utility.visibleIf model.appState.signedIn (homepageIcon model)
        -- , el NavBar [ alignBottom, height (px 30), padding 8 ] (startPageIcon model)
        , (startPageIcon model)
        , Utility.visibleIf model.appState.signedIn (newDocumentButton model)
        , Basic.button "Reader" (Component.activeButton ReaderPage model) [ EE.onClick (GoTo ReaderPage), width (px 60), center]
        , Utility.visibleIf model.appState.signedIn
           (Basic.button "Editor" (Component.activeButton EditorPage model) [ EE.onClick (GoTo EditorPage), width (px 50), center])
        -- , Utility.visibleIf model.appState.signedIn (el (activeButton ImagePage model) [ EE.onClick (GoTo ImagePage), alignBottom, height (px 30), padding 8 ] (text "Image"))
        , Utility.visibleIf (model.current_user.username == "jxxcarlson")
           (Basic.button "Admin" (Component.activeButton AdminPage model) [ EE.onClick (GoTo AdminPage), width (px 55), center])
        ]

authenticationButtonText : Model -> String
authenticationButtonText model =
    if model.appState.signedIn then
        "Sign out"
    else
        "Sign in"

homepageIcon : Model -> Element Styles variation Msg
homepageIcon model =
  Basic.faIcon "Home Page" FontAwesome.home [onClick Types.UserHomePage]

randomDocumentIcon : Model -> Element Styles variation Msg
randomDocumentIcon model =
  Basic.faIcon "Get random documents" FontAwesome.random [onClick RandomDocuments]



userHomePagesIcon : Model -> Element Styles variation Msg
userHomePagesIcon model =
  Basic.faIcon "User Pages" FontAwesome.group  [onClick Types.GotoUserHomePages]
  -- Basic.icon 3 20 "User Pages" Types.GotoUserHomePages FontAwesome.asterisk model
  -- GotoUserHomePages

userPreferencesIcon : Model -> Element Styles variation Msg
userPreferencesIcon model =
  Basic.faIcon "User Preferences" FontAwesome.list_alt  [onClick Types.GotoUserPreferencesPage]

startPageIcon : Model -> Element Styles variation Msg
startPageIcon model =
  Basic.faIcon "Start page" FontAwesome.asterisk [onClick Types.InitHomePage]

newDocumentButton : Model -> Element Styles variation Msg
newDocumentButton model =
  Basic.faIcon "New document" FontAwesome.plus [onClick NewDocument]
