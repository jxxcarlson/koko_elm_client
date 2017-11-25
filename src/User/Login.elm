module User.Login exposing (..)

import Types
    exposing
        ( Model
        , SearchOrder(..)
        , AppState
        , Page
        , Msg(..)
        , Page(..)
        , SearchDomain(..)
        , LoginLocalStorageRecord
        )
import Task
import Time exposing (Time, second)


-- App imports

import Action.Error
import Data.User
import Document.Search
import External
import Initialization
import Request.Api
import Request.Document
import User.Auth
import Views.External


completeRegistration result model =
    case (result) of
        Ok result ->
            let
                user =
                    result.user

                newUser =
                    { name = user.name
                    , username = user.username
                    , id = user.id
                    , email = user.email
                    , password = ""
                    , blurb = ""
                    , token = user.token
                    , admin = False
                    }

                oldAppState =
                    model.appState

                newAppState =
                    { oldAppState | signedIn = True, authorizing = False }
            in
                ( { model | current_user = newUser, appState = newAppState }, Task.perform ReceiveTime Time.now )

        Err err ->
            let
                _ =
                    Debug.log "Registration failure" result
            in
                ( { model | message = Action.Error.httpErrorString err }, Cmd.none )


doLogin model =
    let
        ( model1, cmds1 ) =
            login2 model

        ( model2, cmds2 ) =
            if model1.appState.signedIn then
                ( model1, Cmd.none )
                --Action.Document.search Private "sort=viewed&limit=12" ReaderPage model1
            else
                ( model1, Cmd.none )
    in
        ( model2, Cmd.batch [ cmds1, cmds2 ] )


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

        searchState =
            model.searchState

        newSearchState =
            { searchState | domain = Private }

        user =
            model.current_user

        updatedUser =
            { user | password = "" }
    in
        { model
            | appState = newAppState
            , searchState = newSearchState
            , current_user = updatedUser
        }


login2 : Model -> ( Model, Cmd Msg )
login2 model =
    ( login model, User.Auth.loginUserCmd model Request.Api.loginUrl )



--, Action.Document.search Private "sort=updated" HomePage model


signout : String -> Model -> ( Model, Cmd Msg )
signout message model =
    let
        user =
            model.current_user

        updated_user =
            { name = ""
            , username = ""
            , id = 0
            , email = ""
            , blurb = ""
            , password = ""
            , token = ""
            , admin = False
            }

        oldAppState =
            model.appState

        newAppState =
            { oldAppState | page = Types.HomePage, registerUser = False, signedIn = False, authorizing = False }
    in
        ( Initialization.resetModel model
        , Cmd.batch
            [ External.toJs (Views.External.windowData model HomePage)
            , External.disconnectUser "foo"
            ]
        )


doReconnectUser : String -> Model -> ( Model, Cmd Msg )
doReconnectUser jsonString model =
    let
        _ =
            Debug.log "Enter" "doReconnectUser"

        _ =
            Debug.log "in doReconnectUser, jsonString" jsonString

        maybeUserRecord =
            Data.User.localStorageUserRecord jsonString

        _ =
            Debug.log "maybeUserRecord" maybeUserRecord
    in
        case maybeUserRecord of
            Ok userRecord ->
                let
                    newModel =
                        { model | warning = "" }

                    _ =
                        Debug.log "userRecord" userRecord
                in
                    reconnectUser newModel userRecord

            Err error ->
                ( { model | warning = "Sorry, I cannot reconnect you" }, Cmd.none )


doRecoverUserState : String -> Model -> ( Model, Cmd Msg )
doRecoverUserState jsonString model =
    let
        _ =
            Debug.log "Enter" "doRecoverUserState"

        _ =
            Debug.log "in doRecoverUserState, jsonString" jsonString

        maybeUserStateRecord =
            Debug.log "in doRecoverUserState, maybeUserStateRecord"
                (Data.User.decodeUserStateRecord jsonString)

        _ =
            Debug.log "maybeUserStateRecord" maybeUserStateRecord
    in
        case maybeUserStateRecord of
            Ok userStateRecord ->
                let
                    _ =
                        Debug.log "xxx userStateRecord" userStateRecord

                    idList1 =
                        Debug.log "xxx idList"
                            (userStateRecord.documentIntStack |> List.map toString |> String.join (","))

                    idList =
                        if idList1 == "" then
                            "3"
                        else
                            idList1

                    query =
                        Debug.log "xxx query"
                            ("idlist=" ++ idList)
                in
                    ( model, Request.Document.getDocumentWithQuery LoadDocumentStack query )

            Err error ->
                ( { model | warning = "Sorry, I cannot reconnect you" }, Cmd.none )


reconnectUser : Model -> LoginLocalStorageRecord -> ( Model, Cmd Msg )
reconnectUser model userRecord =
    let
        user =
            model.current_user

        current_user =
            { user
                | username = userRecord.username
                , id = String.toInt userRecord.id |> Result.withDefault 0
                , token = userRecord.token
            }

        searchState =
            model.searchState

        newSearchState =
            { searchState | domain = Private }

        appState =
            model.appState

        newAppState =
            { appState | page = Types.HomePage, signedIn = True, authorizing = False }
    in
        ( { model
            | current_user = current_user
            , appState = newAppState
            , searchState = newSearchState
            , message = "User reconnected: " ++ userRecord.username
          }
        , Cmd.none
        )


shortUsername : Model -> String
shortUsername model =
    let
        shortName =
            model.current_user.username
                |> String.split ("@")
                |> List.head
                |> Maybe.withDefault "---"
    in
        shortName


signOutOrIn model =
    if model.appState.signedIn then
        signout "You are signed out" model
    else
        doSignIn model


doSignIn model =
    let
        appState =
            model.appState

        newAppState =
            { appState | authorizing = True, page = HomePage }
    in
        ( { model | appState = newAppState }, Cmd.none )
