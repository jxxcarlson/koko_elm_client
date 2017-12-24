module Update.Window exposing (update)

import External
import Types exposing (KWindow, Model, Msg(WindowMsg), WindowMsg(..))
import Views.Common
import Views.External


update submessage model =
    case submessage of
        Resize w h ->
            ( updateModel model w h, External.toJs (Views.External.windowData model model.appState.page) )


updateModel : Model -> Int -> Int -> Model
updateModel model w h =
    let
        new_window =
            KWindow w h

        device =
            Views.Common.getDevice w
    in
    { model
        | device = device
        , window = new_window
        , message = "w: " ++ toString model.window.width ++ ", h: " ++ toString model.window.height
    }
