module Update.User exposing (update)

import Action.Error
import Action.User
import Document.Search
import Task
import Types
    exposing
        ( ActiveDocumentList(..)
        , Msg(..)
        , Page(..)
        , SearchDomain(..)
        , SearchOrder(..)
        , UserMsg(..)
        )
import User.Login
import User.Synchronize


-- SetUserState (Result Http.Error ( DocumentsRecord, DocumentsRecord ))


update submessage model =
    case submessage of
        GetUsers (Ok usersRecord) ->
            let
                userList =
                    usersRecord.users

                user =
                    List.head userList |> Maybe.withDefault model.current_user

                query =
                    "authorname=" ++ user.username ++ "&key=home"

                ( model1, cmd ) =
                    Document.Search.withParameters query Alphabetical Public UserHomePages model
            in
            ( { model1 | userList = userList, selectedUserName = user.username }, cmd )

        GetUsers (Err error) ->
            ( { model | message = Action.Error.httpErrorString error }, Cmd.none )

        GetUser (Ok userRecord) ->
            let
                _ =
                    Debug.log "userRecord" "yo!"

                user =
                    userRecord.user

                current_user =
                    model.current_user

                updatedCurrentUser =
                    { current_user | blurb = user.blurb }
            in
            ( { model | current_user = updatedCurrentUser }, Cmd.none )

        GetUser (Err error) ->
            let
                _ =
                    Debug.log "error" error
            in
            ( { model | message = Action.Error.httpErrorString error }, Cmd.none )

        GetUserState (Ok userStateRecord) ->
            let
                _ =
                    Debug.log
                        "in GetUserState"
                        "SUCCESS"

                _ =
                    Debug.log
                        "in GetUserState, userStateRecord"
                        userStateRecord

                appState =
                    model.appState

                searchState =
                    model.searchState

                newSearchState =
                    { searchState | domain = All }

                newAppState =
                    { appState | page = ReaderPage, activeDocumentList = DocumentStackList }

                token =
                    model.current_user.token

                task =
                    User.Synchronize.setUserStateTask userStateRecord token
            in
            ( { model | appState = newAppState, searchState = newSearchState }, Task.attempt (UserMsg << SetUserState) task )

        GetUserState (Err error) ->
            let
                _ =
                    Debug.log
                        "in GetUserState ERROR"
                        (toString error)
            in
            ( model, Cmd.none )

        ReconnectUser jsonString ->
            User.Login.doReconnectUser jsonString model

        RecoverUserState jsonString ->
            User.Synchronize.doRecoverUserState jsonString model

        UpdateCurrentUser ->
            Action.User.updateCurrentUser model

        PutUser (Ok serverReply) ->
            case serverReply of
                () ->
                    ( model, Cmd.none )

        PutUser (Err error) ->
            ( { model | message = Action.Error.httpErrorString error }, Cmd.none )

        SetUserState (Ok result) ->
            User.Synchronize.setUserState result model

        SetUserState (Err err) ->
            ( { model | message = "Error in SetUserState: " ++ toString err }, Cmd.none )
