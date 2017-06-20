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
import Action.UI exposing (appStateWithPage)


registerUserForm : Model -> Element Styles variation Msg
registerUserForm model =
    visibleIf
        ((not model.appState.signedIn)
            && model.appState.authorizing
            && model.appState.registerUser
        )
        (registerUserForm1 model)


registerUserForm1 : Model -> Element Styles variation Msg
registerUserForm1 model =
    column Form
        [ padding 20, spacing 10.0, width (px 320) ]
        [ inputText Field [ EE.onInput Name, placeholder "Name" ] (model.current_user.name)
        , inputText Field [ EE.onInput Username, placeholder "Username" ] (model.current_user.username)
        , inputText Field [ EE.onInput Email, placeholder "Email" ] (model.current_user.email)
        , inputText Field [ EE.onInput Password, placeholder "Password" ] (model.current_user.password)
        , el Button [ EE.onClick Register, alignBottom, height (px 30), width (px 80), padding 8 ] (text "Register")
        , el None [] (text model.info)
        , el Button
            [ onClick ToggleRegister
            , alignBottom
            , height (px 30)
            , width (px 150)
            , padding 8
            ]
            (text "Need to sign in?")
        ]


signinForm : Model -> Element Styles variation Msg
signinForm model =
    visibleIf
        ((not model.appState.signedIn)
            && model.appState.authorizing
            && not model.appState.registerUser
        )
        (signinForm1 model)


signinForm1 : Model -> Element Styles variation Msg
signinForm1 model =
    column Form
        [ padding 20, spacing 10.0, width (px 320) ]
        [ inputText Field [ EE.onInput Email, placeholder "Email" ] (model.current_user.email)
        , inputText Field [ EE.onInput Password, placeholder "Password" ] (model.current_user.password)
        , el Button [ EE.onClick Login, alignBottom, height (px 30), width (px 90), padding 8 ] (text "Sign in")
        , el None [] (text model.info)
        , el Button
            [ onClick ToggleRegister
            , alignBottom
            , height (px 30)
            , width (px 150)
            , padding 8
            ]
            (text "Need to register?")
        ]


signoutForm : Model -> Element Styles variation Msg
signoutForm model =
    visibleIf
        (model.appState.signedIn
            && model.appState.authorizing
        )
        (signoutForm1 model)


signoutForm1 : Model -> Element Styles variation Msg
signoutForm1 model =
    column Form
        [ padding 20, spacing 10.0, width (px 320) ]
        [ el None [] (text ("You are signed in as " ++ model.current_user.username))
        , el Button [ EE.onClick Signout, alignBottom, height (px 30), width (px 90), padding 8 ] (text "Sign out")
        ]


visibleIf : Bool -> Element Styles variation Msg -> Element Styles variation Msg
visibleIf condition body =
    if condition then
        body
    else
        el None [] (text "")


handleAuthentication model =
    if model.appState.signedIn then
        ( { model | appState = appStateWithPage model HomePage }, Cmd.none )
    else
        ( { model | appState = appStateWithPage model HomePage }, Cmd.none )