module Update.Main exposing (update)

import Types exposing (Model, Msg(..), InfoForElm(..    ))
import Update.Auth
import Update.Channel
import Update.Document
import Update.Image
import Update.Page
import Update.Periodic
import Update.Search
import Update.UI
import Update.User
import Update.Window
import Action.Document


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

        Outside infoForElm -> 
            case infoForElm of 
               RenderedText renderedText ->
                 Action.Document.updateRenderedText model renderedText
           

        LogErr errorMessage ->
           ({model | message = "ERROR: " ++ errorMessage}, Cmd.none)
