module Subscriptions exposing (subscriptions)

import External
import Phoenix.Socket
import Time
import Types
    exposing
        ( DocMsg(GetRenderedText)
        , ImageMsg(ImageRead)
        , Model
        , Msg(DocMsg, FileUploaded, ImageMsg, PeriodicMsg, PhoenixMsg, UserMsg, WindowMsg)
        , PeriodicMsg(Tick)
        , UserMsg(ReconnectUser, RecoverUserState)
        , WindowMsg(Resize)
        )
import Window


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every (model.appState.tickInterval * Time.second) (PeriodicMsg << Tick)
        , Window.resizes (\{ width, height } -> WindowMsg (Resize width height))
        , External.reconnectUser (UserMsg << ReconnectUser)
        , External.recoverUserState (UserMsg << RecoverUserState)
        , Phoenix.Socket.listen model.phxSocket PhoenixMsg
        , External.getRenderedText (DocMsg << GetRenderedText) -- pull rendered text from JS-land, then store in DB
        , External.fileContentRead (ImageMsg << ImageRead)
        , External.fileUploaded FileUploaded
        ]
