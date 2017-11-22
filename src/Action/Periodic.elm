module Action.Periodic exposing (do)

import Task
import Time
import Types exposing (Model, Msg(..))
import Action.Channel as Channel


do model =
    let
        ( newModel, cmd1 ) =
            Channel.sendMessage model

        cmd2 =
            Task.perform ReceiveTime Time.now
    in
        ( newModel, Cmd.batch [ cmd1, cmd2 ] )
