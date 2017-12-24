module Main exposing (..)

import Action.Channel
import Action.Document
    exposing
        ( createDocument
        , deleteDocument
        , saveCurrentDocument
        , selectDocument
        , selectNewDocument
        , togglePublic
        , toggleUpdateRate
        , updateCurrentDocumentWithContent
        , updateDocuments
        , updateTags
        )
import Action.Periodic
import Action.Search
import Action.UI
    exposing
        ( appStateToggleAuthorizing
        , appStateWithPage
        , displayPage
        , toggleAuthorizing
        , toggleMenu
        , toggleRegister
        , updateToolStatus
        )
import Date
import External exposing (fileUpload, fileUploaded, putTextToRender, toJs)
import Init exposing (init)
import Nav.Parser exposing (..)
import Navigation
import Phoenix.Socket
import Random
import Task
import Time exposing (Time, second)
import Types exposing (..)
import Update.Auth
import Update.Document
import Update.Image exposing (update)
import Update.Page
import Update.Periodic
import Update.Search
import Update.User
import User.Login
import User.Synchronize
import Views.Common as Common
import Views.External exposing (windowData, windowSetup)
import Views.Main exposing (view)
import Views.TOC as TOC exposing (toggleListView)
import Window exposing (..)


main : Program Flags Model Msg
main =
    Navigation.programWithFlags urlParser
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


updateWindow : Model -> Int -> Int -> Model
updateWindow model w h =
    let
        new_window =
            KWindow w h

        device =
            Common.getDevice w
    in
    { model
        | device = device
        , window = new_window
        , message = "w: " ++ toString model.window.width ++ ", h: " ++ toString model.window.height
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( { model | message = "NoOp" }, Cmd.none )

        AuthMsg submessage ->
            Update.Auth.update submessage model

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

        UserMsg submessage ->
            Update.User.update submessage model

        ToggleListView ->
            TOC.toggleListView model

        ToggleUpdateRate ->
            ( Action.Document.toggleUpdateRate model, Cmd.none )

        ToggleMenu menu ->
            toggleMenu menu model

        DoSearch searchDomain key ->
            Action.Search.doSearch searchDomain key model

        -- Document.Search.withParameters searchTerm Alphabetical Public ReaderPage model
        MigrateFromAsciidocLatex ->
            Action.Document.migrateFromAsciidocLatex model

        -- User.Login.signout "Error: could not get user documents." model
        -- ( { model | message = "Error, cannot get documents" }, Cmd.none )
        Message str ->
            ( { model | message = str }, Cmd.none )

        Resize w h ->
            ( updateWindow model w h, toJs (Views.External.windowData model model.appState.page) )

        SelectTool t ->
            ( { model | appState = updateToolStatus model t }, Cmd.none )

        SetUserState (Ok result) ->
            User.Synchronize.setUserState result model

        SetUserState (Err err) ->
            ( { model | message = "Error in SetUserState: " ++ toString err }, Cmd.none )

        Files nativeFiles ->
            ( { model | fileToUpload = List.head nativeFiles }, Cmd.none )

        -----
        UploadComplete (Ok result) ->
            ( model, Cmd.none )

        UploadComplete (Err error) ->
            ( model, Cmd.none )

        FileSelected ->
            ( model, fileUpload model.fileInputId )

        FileUploaded True ->
            -- obviously, set some state notifying success
            ( model, Cmd.none )

        FileUploaded False ->
            -- obviously, set some state notifying failure
            ( model, Cmd.none )

        -- (model, Cmd.none) --
        SendToJS str ->
            ( model, toJs str )

        SetMessage message ->
            Action.Channel.setMessage message model

        -- ( { model | messageInProgress = message }, Cmd.none )
        SendMessage ->
            Action.Channel.sendMessage model

        ReceiveChatMessage raw ->
            Action.Channel.receiveRaw raw model

        -- ( { model | messages = "Failed to receive message" :: model.messages }, Cmd.none )
        HandleSendError err ->
            Action.Channel.handlePing False model

        PhoenixMsg msg ->
            Action.Channel.handleMsg msg model

        LinkTo path ->
            ( model, Navigation.newUrl path )

        GenerateSeed ->
            ( model, Random.generate NewSeed (Random.int 1 10000) )

        NewSeed newSeed ->
            let
                appState =
                    model.appState

                newAppState =
                    { appState | seed = Debug.log "newSeed" newSeed }
            in
            ( { model | appState = newAppState }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every (model.appState.tickInterval * Time.second) (PeriodicMsg << Tick)
        , Window.resizes (\{ width, height } -> Resize width height)
        , External.reconnectUser (UserMsg << ReconnectUser)
        , External.recoverUserState (UserMsg << RecoverUserState)
        , Phoenix.Socket.listen model.phxSocket PhoenixMsg
        , External.getRenderedText (DocMsg << GetRenderedText) -- pull rendered text from JS-land, then store in DB
        , External.fileContentRead (ImageMsg << ImageRead)
        , fileUploaded FileUploaded
        ]
