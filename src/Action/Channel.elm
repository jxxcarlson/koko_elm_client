module Action.Channel exposing (..)

import Types exposing (..)
import Phoenix.Socket
import Phoenix.Channel
import Phoenix.Push
import Json.Encode as JsEncode
import Json.Decode as JsDecode


setMessage : String -> Model -> ( Model, Cmd msg )
setMessage message model =
    ( { model | messageInProgress = message }, Cmd.none )


sendMessage : Model -> ( Model, Cmd Msg )
sendMessage model =
    let
        payload =
            JsEncode.object
                [ ( "message", JsEncode.string model.messageInProgress )
                ]

        phxPush =
            Phoenix.Push.init "shout" "room:lobby"
                |> Phoenix.Push.withPayload payload
                |> Phoenix.Push.onOk ReceiveChatMessage
                |> Phoenix.Push.onError HandleSendError

        ( phxSocket, phxCmd ) =
            Phoenix.Socket.push phxPush model.phxSocket
    in
        ( { model
            | messageInProgress = ""
            , phxSocket = phxSocket
          }
        , Cmd.map PhoenixMsg phxCmd
        )


sendImmediateMessage : String -> Model -> ( Model, Cmd Msg )
sendImmediateMessage message model =
    let
        payload =
            JsEncode.object
                [ ( "message", JsEncode.string message )
                ]

        phxPush =
            Phoenix.Push.init "hello" "room:lobby"
                |> Phoenix.Push.withPayload payload
                |> Phoenix.Push.onOk ReceiveChatMessage
                |> Phoenix.Push.onError HandleSendError

        ( phxSocket, phxCmd ) =
            Phoenix.Socket.push phxPush model.phxSocket
    in
        ( { model
            | messageInProgress = ""
            , phxSocket = phxSocket
          }
        , Cmd.map PhoenixMsg phxCmd
        )


handlePing : Bool -> Model -> ( Model, Cmd Msg )
handlePing value model =
    let
        appState =
            model.appState

        updatedAppState =
            { appState | online = value }
    in
        ( { model | appState = updatedAppState }, Cmd.none )


joinChannel model =
    let
        channel =
            Phoenix.Channel.init "room:lobby"

        ( initSocket, phxCmd ) =
            Phoenix.Socket.init "ws://localhost:4000/socket/websocket"
                |> Phoenix.Socket.withDebug
                |> Phoenix.Socket.on "shout" "room:lobby" ReceiveChatMessage
                |> Phoenix.Socket.join channel
    in
        ( { model | phxSocket = initSocket }, Cmd.map PhoenixMsg phxCmd )
