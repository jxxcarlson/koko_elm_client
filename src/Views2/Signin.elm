module Views2.Signin exposing (..)

import Style exposing (..)
import StyleSheet exposing (..)
import Color
import Element exposing (..)
import Element.Attributes as EA exposing (..)
import Element.Events as EE exposing (..)
import Style exposing (..)
import Style.Border as Border
import Style.Color as Color
import Style.Font as Font
import Style.Transition as Transition
import Types exposing (..)


registerUserForm : Model -> Element Styles variation Msg
registerUserForm model =
    column Form
        [ padding 20, spacing 10.0, width (px 320) ]
        [ inputText Field [ EE.onInput Name, placeholder "Name" ] ("")
        , inputText Field [ EE.onInput Username, placeholder "Username" ] ("")
        , inputText Field [ EE.onInput Email, placeholder "Email" ] ("")
        , inputText Field [ EE.onInput Password, placeholder "Password" ] ("")
        , el Button [ EE.onClick Register, alignBottom, height (px 30), width (px 80), padding 8 ] (text "Register")
        , el None [] (text model.message)
        , el None [] (text model.info)
        ]



-- registerUserForm : Model -> Html Msg
-- registerUserForm model =
--     div [ id "loginForm" ]
--         [ input [ id "name", type_ "text", placeholder "Your name", onInput Name ] []
--         , br [] []
--         , br [] []
--         , input [ id "username", type_ "text", placeholder "Username", onInput Username ] []
--         , br [] []
--         , br [] []
--         , input [ id "email", type_ "text", placeholder "Email", onInput Email ] []
--         , br [] []
--         , br [] []
--         , input [ id "password", type_ "text", placeholder "Password", onInput Password ] []
--         , br [] []
--         , br [] []
--         , button [ id "loginButton", onClick Register ] [ text "Sign up" ]
--         , br [] []
--         , br [] []
--         , p [] [ text model.info ]
--         ]
