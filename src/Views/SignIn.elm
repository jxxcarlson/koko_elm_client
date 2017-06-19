module Views.Signin exposing (signin)

import Types exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events as HE exposing (onClick, onInput)
import Json.Decode exposing (int, list, string, float, Decoder)


signin : Model -> Html Msg
signin model =
    if model.current_user.token == "" then
        if model.appState.registerUser == True then
            registerUserView model
        else
            signinView model
    else
        signoutView model


signinView : Model -> Html Msg
signinView model =
    div []
        [ div []
            [ div [ id "login" ]
                [ h3 [] [ text "Sign in" ]
                , signinForm model
                , br [] []
                , button [ onClick ToggleRegister, class "wide_button" ] [ text "Need to register?" ]
                ]
            ]
        , div [ id "carbonAd" ] [ text "Carbon A" ]
        ]


signoutView : Model -> Html Msg
signoutView model =
    div []
        [ div [ id "login" ]
            [ div [ id "loginForm2" ]
                [ p [ id "username" ] [ text ("Signed in as " ++ model.current_user.username) ]
                , br [] []
                , br [] []
                , button [ id "logoutButton", onClick Signout ] [ text "Sign out" ]
                ]
            ]
        , div [ id "carbonAd2" ] [ text "Carbon B" ]
        ]


registerUserView : Model -> Html Msg
registerUserView model =
    div []
        [ div []
            [ div [ id "login" ]
                [ h3 [] [ text "Sign up" ]
                , registerUserForm model
                , br [] []
                , button [ onClick ToggleRegister, class "wide_button" ] [ text "Need to sign in?" ]
                ]
            ]
        , div [ id "carbonAd" ] [ text "Carbon C" ]
        ]


signinForm : Model -> Html Msg
signinForm model =
    div [ id "loginForm" ]
        [ input [ id "email", type_ "text", placeholder "Email", onInput Email ] []
        , br [] []
        , br [] []
        , input [ id "password", type_ "text", placeholder "Password", onInput Password ] []
        , br [] []
        , br [] []
        , button [ id "loginButton", onClick Login ] [ text "Sign in" ]
        , br [] []
        , br [] []
        , p [] [ text model.info ]
        ]


registerUserForm : Model -> Html Msg
registerUserForm model =
    div [ id "loginForm" ]
        [ input [ id "name", type_ "text", placeholder "Your name", onInput Name ] []
        , br [] []
        , br [] []
        , input [ id "username", type_ "text", placeholder "Username", onInput Username ] []
        , br [] []
        , br [] []
        , input [ id "email", type_ "text", placeholder "Email", onInput Email ] []
        , br [] []
        , br [] []
        , input [ id "password", type_ "text", placeholder "Password", onInput Password ] []
        , br [] []
        , br [] []
        , button [ id "loginButton", onClick Register ] [ text "Sign up" ]
        , br [] []
        , br [] []
        , p [] [ text model.info ]
        ]
