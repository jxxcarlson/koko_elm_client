module Update.Window exposing (update)

import External
import Types exposing (KWindow, Model, Msg(WindowMsg), WindowMsg(..), InfoForOutside(WindowData))
import Views.Common
import Views.External
import OutsideInfo


update submessage model =
    case submessage of
        Resize w h ->
            ( updateModel model w h, OutsideInfo.sendInfoOutside (WindowData <| Views.External.encodeWindowData model model.appState.page) )


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
