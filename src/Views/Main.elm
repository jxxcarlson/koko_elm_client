module Views.Main exposing (view)

import Element as EL exposing (..)
import Element.Attributes as EA exposing (..)
import Html exposing (Html)
import Image.View
import StyleSheet exposing (..)
import Types exposing (Model, Msg, Page(..))
import Views.Admin exposing (admin)
import Views.Editor exposing (editor)
import Views.Footer as Footer
import Views.Home exposing (home)
import Views.Login exposing (loginPage)
import Views.NavBar as NavBar
import Views.Reader exposing (reader)
import Views.UserHomePages exposing (userHomePages)
import Views.UserPreferences exposing (userPreferences)


view : Model -> Html Msg
view model =
    EL.root StyleSheet.stylesheet <|
        column None
            []
            [ NavBar.navigation model
            , hairline Hairline
            , el None [ center, EA.width (percent 100) ] <|
                column Main
                    [ spacing 0 ]
                    (List.concat
                        [ page model ]
                    )
            , screen (Footer.footer model)
            ]


page : Model -> List (Element Styles variation Msg)
page model =
    case model.appState.page of
        ReaderPage ->
            reader model

        PublicPage _ ->
            reader model

        PrivatePage _ ->
            reader model

        EditorPage ->
            editor model

        ImagePage ->
            Image.View.imageEditor model

        StartPage ->
            home model

        LoginPage ->
            loginPage model

        AdminPage ->
            admin model

        UserHomePages ->
            userHomePages model

        UserPreferencesPage ->
            userPreferences model
