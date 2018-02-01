module User.Display exposing (..)

import Element as EL exposing (..)
import Element.Attributes as EA exposing (..)
import Element.Events as EE exposing (..)
import StyleSheet exposing (..)
import Types
    exposing
        ( Model
        , Msg(..)
        , Page(..)
        , PageMsg(GetHomePageForUserHomePages, GetPublicPage)
        , SearchDomain(..)
        , SearchOrder(..)
        , User
        , Users
        )
import User.Request
import Views.Utility as Utility


list : String -> Model -> Element Styles variation Msg
list title model =
    column None
        [ height (percent 100) ]
        [ userListHeader title model
        , userListView1 title model
        ]


userListView1 : String -> Model -> Element Styles variation Msg
userListView1 title model =
    column PaleBlue
        [ yScrollbar, paddingTop 15, spacing 0, width (px 300), height (px (toFloat (model.window.height - 200))) ]
        (List.map (viewUser model) model.userList)


userListHeader : String -> Model -> Element Styles variation Msg
userListHeader title model =
    el HeadingAlternate [ height (px 30), paddingXY 8 4 ] (text "Home Pages")


viewUser : Model -> User -> Element Styles variation Msg
viewUser model user =
    let
        query =
            "authorname=" ++ user.username ++ "&key=home"
    in
    el (titleStyle model user)
        [ onClick (PageMsg (GetHomePageForUserHomePages query user.username))
        , spacing 15
        , paddingLeft 10
        , paddingTop 5
        , paddingBottom 2.5
        , height (px 30)
        , width (px 500)
        ]
        (text (Utility.shortString 35 (user.username ++ " (" ++ toString user.id ++ "): " ++ user.blurb)))


titleStyle : Model -> User -> Styles
titleStyle model user =
    if model.selectedUserName == user.username then
        Blue
    else
        PaleBlue


goToUserHomePages : Model -> ( Model, Cmd Msg )
goToUserHomePages model =
    let
        appState =
            model.appState

        newAppState =
            { appState | page = UserHomePages }
    in
    ( { model | appState = newAppState }, User.Request.getList "" )
