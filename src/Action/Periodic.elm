module Action.Periodic exposing (do)

import Action.Channel as Channel
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


do model time =
    let
        integerTick =
            round (time / 1000.0)

        intervalSinceLastEdit =
            -- interval in seconds
            case model.lastEditTime of
                Just lastEditTime ->
                    (time - lastEditTime) / 1000.0

                Nothing ->
                    0

        _ =
            Debug.log "Last Edit Interval" ( integerTick % 1000, intervalSinceLastEdit )

        appState =
            model.appState

        page =
            if intervalSinceLastEdit > 600.0 then
                ReaderPage
            else
                appState.page

        newAppState =
            { appState | page = page }

        ( model1, cmd1 ) =
            Channel.sendMessage model

        newUserStateRecord =
            computeUserStateRecord model

        cmd3 =
            if doUpdateUserRecord newUserStateRecord model then
                User.Request.putUserStateRecord newUserStateRecord model
            else
                Cmd.none

        model2 =
            { model1 | appState = newAppState, userStateRecord = computeUserStateRecord model }

        cmd2 =
            Task.perform ReceiveTime Time.now
    in
    ( model2, Cmd.batch [ cmd1, cmd2, cmd3 ] )



-- User.Request.putUserState model
