module Update.Periodic exposing (update)

import Action.Channel
import Action.Document
import Action.Periodic
import Date
import Jwt
import Task
import Time
import Types exposing (Msg(PeriodicMsg), Page(EditorPage), PeriodicMsg(..))
import User.Login


update submessage model =
    case submessage of
        Tick time ->
            let
                tick =
                    model.tick + 1

                newModel =
                    { model | tick = tick }
            in
            if
                model.appState.page
                    == EditorPage
                    && model.appState.textBufferDirty
                    && model.current_document.attributes.docType
                    /= "master"
            then
                if model.current_document.attributes.textType == "latex" then
                    Action.Document.saveCurrentDocument "" newModel
                else
                    Action.Document.updateCurrentDocumentWithContent newModel
            else if model.appState.online then
                Action.Periodic.do newModel time
            else
                Action.Channel.joinChannel newModel

        RequestDate ->
            ( model, Task.perform (PeriodicMsg << ReceiveDate) Date.now )

        ReceiveDate date ->
            let
                nextModel =
                    { model | date = Just date }
            in
            ( nextModel, Cmd.none )

        RequestTime ->
            ( model, Task.perform (PeriodicMsg << ReceiveTime) Time.now )

        ReceiveTime time ->
            let
                time_ =
                    Just time

                token =
                    model.current_user.token

                ( expired, message ) =
                    if token /= "" then
                        case Jwt.isExpired time token of
                            Ok False ->
                                ( False, "Session valid" )

                            Ok True ->
                                ( True, "Session expired" )

                            Err error ->
                                ( True, "Session expired (2)" )
                    else
                        ( False, "Not signed in" )

                ( newModel, cmd ) =
                    if expired then
                        User.Login.signout "You are now signed out." model
                    else
                        ( model, Cmd.none )

                _ =
                    Debug.log "TOK" message
            in
            ( { newModel | message = message, time = time_ }, cmd )
