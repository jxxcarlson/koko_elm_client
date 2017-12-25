module Main exposing (..)

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
import External exposing (fileUpload, fileUploaded, putTextToRender, toJs)
import Init exposing (init)
import Nav.Parser
import Navigation
import Random
import Subscriptions exposing (subscriptions)
import Types exposing (..)
import Update.Auth
import Update.Channel
import Update.Document
import Update.Image exposing (update)
import Update.Page
import Update.Periodic
import Update.Search
import Update.User
import Update.Window
import Views.Main exposing (view)
import Views.TOC as TOC exposing (toggleListView)


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

        UserMsg submessage ->
            Update.User.update submessage model

        WindowMsg submessage ->
            Update.Window.update submessage model

        ToggleListView ->
            TOC.toggleListView model

        ToggleUpdateRate ->
            ( Action.Document.toggleUpdateRate model, Cmd.none )

        ToggleMenu menu ->
            toggleMenu menu model

        -- Document.Search.withParameters searchTerm Alphabetical Public ReaderPage model
        MigrateFromAsciidocLatex ->
            Action.Document.migrateFromAsciidocLatex model

        Message str ->
            ( { model | message = str }, Cmd.none )

        SelectTool t ->
            ( { model | appState = updateToolStatus model t }, Cmd.none )

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
