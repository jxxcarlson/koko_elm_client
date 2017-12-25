module Main exposing (..)

import External exposing (fileUpload, fileUploaded, putTextToRender, toJs)
import Init exposing (init)
import Nav.Parser
import Navigation
import Subscriptions exposing (subscriptions)
import Types exposing (..)
import Update.Auth
import Update.Channel
import Update.Document
import Update.Image exposing (update)
import Update.Page
import Update.Periodic
import Update.Search
import Update.UI
import Update.User
import Update.Window
import Views.Main exposing (view)


main : Program Flags Model Msg
main =
    Navigation.programWithFlags Nav.Parser.urlParser
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( { model | message = "NoOp" }, Cmd.none )

        AuthMsg submessage ->
            Update.Auth.update submessage model

        ChannelMsg submessage ->
            Update.Channel.update submessage model

        DocMsg submessage ->
            Update.Document.update submessage model

        ImageMsg submessage ->
            Update.Image.update submessage model

        PageMsg submessage ->
            Update.Page.update submessage model

        PeriodicMsg submessage ->
            Update.Periodic.update submessage model

        SearchMsg submessage ->
            Update.Search.update submessage model

        UIMsg submessage ->
            Update.UI.update submessage model

        UserMsg submessage ->
            Update.User.update submessage model

        WindowMsg submessage ->
            Update.Window.update submessage model



-- ----
-- SendToJS str ->
--     ( model, toJs str )
-- LinkTo path ->
--     ( model, Navigation.newUrl path )
