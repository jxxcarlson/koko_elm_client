module Action.Periodic exposing (do)

import Task
import Time
import Types exposing (Model, Msg(..), Page(..))
import Action.Channel as Channel


do model time =
    let
        intervalSinceLastEdit =
            -- interval in seconds
            case model.lastEditTime of
                Just lastEditTime ->
                    (time - lastEditTime) / 1000.0

                Nothing ->
                    0

        _ =
            Debug.log "LE INTERVAL" intervalSinceLastEdit

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
    in
        ( model2, Cmd.batch [ cmd1, cmd2 ] )
