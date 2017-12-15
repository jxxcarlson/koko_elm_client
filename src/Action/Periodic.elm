module Action.Periodic exposing (do)

import Action.Channel as Channel
import Action.Document
import Task
import Time
import Types exposing (Model, Msg(..), Page(..), UserStateRecord)
import User.Request


computeUserStateRecord : Model -> UserStateRecord
computeUserStateRecord model =
    { documentIntStack = List.map (\doc -> doc.id) model.documentStack
    , currentDocumentId = Ok model.current_document.id
    , token = model.current_user.token
    }


doUpdateUserRecord : UserStateRecord -> Model -> Bool
doUpdateUserRecord userStateRecord model =
    model.userStateRecord /= userStateRecord


gotoReader time model =
    let
        intervalSinceLastEdit =
            -- interval in seconds
            case model.lastEditTime of
                Just lastEditTime ->
                    (time - lastEditTime) / 1000.0

                Nothing ->
                    0

        appState =
            model.appState

        newPage =
            if intervalSinceLastEdit > 600.0 then
                ReaderPage
            else
                appState.page

        newAppState =
            { appState | page = newPage }
    in
    ( { model | appState = newAppState }, Cmd.none )


sendChannelMessage model =
    Channel.sendMessage model


updateUserState model =
    let
        newUserStateRecord =
            computeUserStateRecord model

        cmd =
            Task.perform ReceiveTime Time.now
    in
    if doUpdateUserRecord newUserStateRecord model then
        ( model, Cmd.batch [ User.Request.putUserStateRecord newUserStateRecord model, cmd ] )
    else
        ( model, cmd )


documentNeedsUpdate : Model -> Bool
documentNeedsUpdate model =
    model.appState.page
        == EditorPage
        && model.appState.textBufferDirty
        && model.current_document.attributes.docType
        /= "master"
        && model.current_document.attributes.textType
        /= "latex"


saveLatexDocument model =
    if (model.current_document.attributes.textType == "latex") && model.appState.textBufferDirty then
        Action.Document.saveCurrentDocument "" model
    else
        ( model, Cmd.none )


doCycle model time =
    let
        cycle =
            Debug.log "cycle"
                (model.tick % 5)

        cmd2 =
            Task.perform ReceiveTime Time.now
    in
    case cycle of
        0 ->
            updateUserState model

        1 ->
            sendChannelMessage model

        2 ->
            Channel.joinChannel model

        3 ->
            saveLatexDocument model

        _ ->
            gotoReader time model


do model time =
    let
        _ =
            Debug.log "Cycle" -1
    in
    if documentNeedsUpdate model then
        -- doCycle model cycle time
        Action.Document.updateCurrentDocumentWithContent model
    else
        doCycle model time



-- User.Request.putUserState model
