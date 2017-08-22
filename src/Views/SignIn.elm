module Views.Signin exposing (..)


import StyleSheet exposing (..)

import Element exposing (..)
import Element.Attributes as EA exposing (..)
import Element.Events as EE exposing (..)
import Types exposing (..)
import Action.UI exposing (appStateWithPage)
import Views.Basic as Basic
import Views.Component as Component
import Views.Utility as Utility


registerUserForm : Model -> Element Styles variation Msg
registerUserForm model =
    Utility.visibleIf
        ((not model.appState.signedIn)
            && model.appState.authorizing
            && model.appState.registerUser
        )
        (registerUserForm1 model)


registerUserForm1 : Model -> Element Styles variation Msg
registerUserForm1 model =
    column Blue
        [ padding 20, spacing 10.0, width (px 320), height (px 400) ]
        [ inputText Field [ EE.onInput Name, placeholder "Name" ] (model.current_user.name)
        , inputText Field [ EE.onInput Username, placeholder "Username" ] (model.current_user.username)
        , inputText Field [ EE.onInput Email, placeholder "Email" ] (model.current_user.email)
        , inputText Field [ EE.onInput Password, placeholder "Password" ] (model.current_user.password)
        , Basic.button "Register" Button [onClick Register, width (px 80)]
        -- l Button [ EE.onClick Register, alignBottom, height (px 30), width (px 80), padding 8 ] (text "Register")
        , Basic.button "Toggle" Button [onClick ToggleRegister, width (px 150)]
        -- el Button
        --     [ onClick ToggleRegister
        --     , alignBottom
        --     , height (px 30)
        --     , width (px 150)
        --     , padding 8
        --     ]
        --     (text "Need to sign in?")
        ,    Component.cancelAuthentication Button model
        , el Blue [paddingXY 20 40] (text model.message)
        ]



signinForm : Model -> Element Styles variation Msg
signinForm model =
    Utility.visibleIf
        ((not model.appState.signedIn)
            && model.appState.authorizing
            && not model.appState.registerUser
        )
        (signinForm1 model)


signinForm1 : Model -> Element Styles variation Msg
signinForm1 model =
    column Blue
        [ padding 20, spacing 10.0, width (px 320), height (px 400) ]
        [ inputText Field [ EE.onInput Email, placeholder "Email" ] (model.current_user.email)
        , inputText Field [ EE.onInput Password, placeholder "Password" ] (model.current_user.password)
        , el Button [ EE.onClick Login, alignBottom, height (px 30), width (px 90), padding 8 ] (text "Sign in")
        , el Button
            [ onClick ToggleRegister
            , alignBottom
            , height (px 30)
            , width (px 150)
            , padding 8
            ]
            (text "Need to register?")
        , Component.cancelAuthentication Button model
        , el Blue [paddingXY 20 40] (text model.message)
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
        [ padding 20, spacing 10.0, width (px 320), height (px 400) ]
        [ (text ("You are signed in as " ++ model.current_user.username))
        , el Button [ EE.onClick Signout, alignBottom, height (px 30), width (px 90), padding 8 ] (text "Sign out")
        ]

signinInfoPanel : Model -> Element Styles variation Msg
signinInfoPanel model =
  Utility.notVisibleIf
      (model.appState.authorizing || model.appState.signedIn)
      (signinInfoPanel1 model)

signinInfoPanel1 : Model -> Element Styles variation Msg
signinInfoPanel1 model =
  (column Box
      [ height (px 260), paddingXY 20 40 ]
      [ el Zero [ width (px 320), height (px 40) ] (text "")
      , Component.loginButton Button model
      , el Zero [height (px 20)] (text "")
      , Component.cancelAuthentication Button model
      , el Blue [paddingXY 0 40] (text model.message)
      ]
    )



handleAuthentication : Model -> (Model, Cmd Msg)
handleAuthentication model =
    if model.appState.signedIn then
        ( { model | appState = appStateWithPage model HomePage }, Cmd.none )
    else
        ( { model | appState = appStateWithPage model HomePage }, Cmd.none )
