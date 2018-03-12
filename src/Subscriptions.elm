module Subscriptions exposing (subscriptions)

import External
import Phoenix.Socket
import Time
import Types
    exposing
        ( ChannelMsg(PhoenixMsg)
        , ErrorMessage
        , ImageMsg(FileUploaded, ImageRead)
        , Model
        , Msg(ChannelMsg, DocMsg, ImageMsg, PeriodicMsg, UserMsg, WindowMsg, Outside, LogErr    )
        , PeriodicMsg(Tick)
        , WindowMsg(Resize) 
        )
import Window
import OutsideInfo


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every (model.appState.tickInterval * Time.second) (PeriodicMsg << Tick)
        , Window.resizes (\{ width, height } -> WindowMsg (Resize width height))
        , Phoenix.Socket.listen model.phxSocket (ChannelMsg << PhoenixMsg)
        , OutsideInfo.getInfoFromOutside Outside LogErr
        , External.fileContentRead (ImageMsg << ImageRead)
        , External.fileUploaded (ImageMsg << FileUploaded)
        ]
