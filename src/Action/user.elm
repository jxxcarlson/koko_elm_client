module Action.User exposing (..)

import Types exposing (..)


updateEmail : Model -> String -> ( Model, Cmd Msg )
updateEmail model email =
    let
        user =
            model.current_user

        updated_user =
            { user | email = email }
    in
        ( { model | current_user = updated_user }, Cmd.none )


updatePassword : Model -> String -> ( Model, Cmd Msg )
updatePassword model password =
    let
        user =
            model.current_user

        updated_user =
            { user | password = password }
    in
        ( { model | current_user = updated_user }, Cmd.none )


updateName : Model -> String -> ( Model, Cmd Msg )
updateName model name =
    let
        user =
            model.current_user

        updated_user =
            { user | name = name }
    in
        ( { model | current_user = updated_user }, Cmd.none )


updateUsername : Model -> String -> ( Model, Cmd Msg )
updateUsername model username =
    let
        user =
            model.current_user

        updated_user =
            { user | username = username }
    in
        ( { model | current_user = updated_user }, Cmd.none )


login : Model -> Model
login model =
    let
        appState =
            model.appState

        newAppState =
            { appState | page = Types.HomePage, signedIn = True, authorizing = False }

        user =
            model.current_user

        updatedUser =
            { user | password = "" }
    in
        { model | appState = newAppState, current_user = updatedUser }


signout : Model -> ( Model, Cmd Msg )
signout model =
    let
        user =
            model.current_user

        updated_user =
            User "" "" "" "" ""

        oldAppState =
            model.appState

        newAppState =
            { oldAppState | page = Types.HomePage, registerUser = False, signedIn = False, authorizing = False }
    in
        ( { model
            | current_user = updated_user
            , appState = newAppState
            , info = ""
            , message = "Please sign in"
          }
        , Cmd.none
        )


reconnectUser : Model -> UserRecord -> ( Model, Cmd Msg )
reconnectUser model userRecord =
    let
        user =
            model.current_user

        current_user =
            { user
                | username = userRecord.username
                , token = userRecord.token
            }

        appState =
            model.appState

        newAppState =
            { appState | page = Types.HomePage, signedIn = True, authorizing = False }
    in
        ( { model
            | current_user = current_user
            , appState = newAppState
            , message = "User reconnected: " ++ userRecord.username
          }
        , Cmd.none
        )
