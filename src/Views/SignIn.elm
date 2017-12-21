module Views.Signin exposing (..)

import Action.UI exposing (appStateWithPage)
import Element exposing (..)
import Element.Attributes as EA exposing (..)
import Element.Events as EE exposing (..)
import StyleSheet exposing (..)
import Types exposing (..)
import Views.Basic as Basic
import Views.Component as Component
import Views.NavBar as NavBar
import Views.Utility as Utility


registerUserForm : Model -> Element Styles variation Msg
registerUserForm model =
    Utility.visibleIf
        (not model.appState.signedIn
            && model.appState.authorizing
            && model.appState.registerUser
        )
        (registerUserForm1 model)


registerUserForm1 : Model -> Element Styles variation Msg
registerUserForm1 model =
    column Blue
        [ padding 20, spacing 10.0, width (px 320), height (px 400) ]
        [ inputText Field [ EE.onInput (AuthMsg << Name), placeholder "Name" ] model.current_user.name
        , inputText Field [ EE.onInput Username, placeholder "Username" ] model.current_user.username
        , inputText Field [ EE.onInput Email, placeholder "Email" ] model.current_user.email
        , inputText Field [ EE.onInput (AuthMsg << Password), placeholder "Password" ] model.current_user.password
        , Basic.button "Register" Button [ onClick (AuthMsg Register), width (px 80) ]
        , Basic.button "Toggle" Button [ onClick ToggleRegister, width (px 150) ]
        , Component.cancelAuthentication Button model
        , el Blue [ paddingXY 20 40 ] (text model.message)
        ]


signinForm : Model -> Element Styles variation Msg
signinForm model =
    Utility.visibleIf
        (not model.appState.signedIn
            && model.appState.authorizing
            && not model.appState.registerUser
        )
        (signinForm1 model)



-- node "input" <| el None [ type_ "password", value "secret" ] empty


signinForm1 : Model -> Element Styles variation Msg
signinForm1 model =
    column Blue
        [ padding 20, spacing 10.0, width (px 320), height (px 400) ]
        [ inputText Field [ EE.onInput Email, placeholder "Email" ] model.current_user.email
        , node "input" <| el None [ type_ "password", onInput (AuthMsg << Password), placeholder "Password", value model.current_user.password ] empty
        , el Button [ EE.onClick (AuthMsg Login), alignBottom, height (px 30), width (px 90), padding 8 ] (text "Sign in")
        , el Button
            [ onClick ToggleRegister
            , alignBottom
            , height (px 30)
            , width (px 150)
            , padding 8
            ]
            (text "Need to register?")
        , Component.cancelAuthentication Button model
        , el Blue [ paddingXY 20 40 ] (text model.message)
        ]


signoutForm : Model -> Element Styles variation Msg
signoutForm model =
    Utility.visibleIf
        (model.appState.signedIn
            && model.appState.authorizing
        )
        (signoutForm1 model)


signoutForm1 : Model -> Element Styles variation Msg
signoutForm1 model =
    column PaleBlue
        [ padding 20, spacing 10.0, width (px 380), height (px 700) ]
        [ text ("You are signed in as " ++ model.current_user.username)
        , el Button [ EE.onClick (AuthMsg Signout), alignBottom, height (px 30), width (px 90), padding 8 ] (text "Sign out")
        ]


signinInfoPanel : Model -> Element Styles variation Msg
signinInfoPanel model =
    Utility.notVisibleIf
        (model.appState.authorizing || model.appState.signedIn)
        (signinInfoPanel1 model)


signinInfoPanel1 : Model -> Element Styles variation Msg
signinInfoPanel1 model =
    column Box
        [ height (px 260), paddingXY 20 40 ]
        [ el Zero [ width (px 320), height (px 40) ] (text "")
        , NavBar.loginButton Button model
        , el Zero [ height (px 20) ] (text "")
        , Component.cancelAuthentication Button model
        , el Blue [ paddingXY 0 40 ] (text model.message)
        ]


handleAuthentication : Model -> ( Model, Cmd Msg )
handleAuthentication model =
    if model.appState.signedIn then
        ( { model | appState = appStateWithPage model StartPage }, Cmd.none )
    else
        ( { model | appState = appStateWithPage model StartPage }, Cmd.none )
