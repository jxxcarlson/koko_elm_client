module Action.Periodic exposing (do)

import Action.Channel as Channel
import Task
import Time
import Types exposing (Model, Msg(..), Page(..))
import User.Request


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

        model2 =
            { model1 | appState = newAppState }

        cmd2 =
            Task.perform ReceiveTime Time.now

        cmd3 =
            if (integerTick % 30) == 2 then
                User.Request.putUserState model
            else
                Cmd.none
    in
    ( model2, Cmd.batch [ cmd1, cmd2, cmd3 ] )



-- User.Request.putUserState model
