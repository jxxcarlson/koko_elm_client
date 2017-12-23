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
import Action.Error
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
import Action.User
import Configuration
import Date exposing (Date)
import Dict
import Document.Dictionary
import Document.Document as Document
    exposing
        ( blankDocument
        , defaultDocument
        , defaultMasterDocument
        , diaryEntry
        , emptyDocument
        , startDocument
        )
import Document.MasterDocument
import Document.Render
import Document.Search
import Document.TOC
import Element as EL exposing (..)
import Element.Attributes as EA exposing (..)
import External exposing (fileUpload, fileUploaded, putTextToRender, toJs)
import Html exposing (..)
import Image.Upload
import Image.View
import Jwt
import MiniLatex.Differ exposing (EditRecord, emptyEditRecord)
import Nav.Parser exposing (..)
import Nav.UrlParseExtra as Url
import Navigation
import Parser
import Phoenix.Channel
import Phoenix.Socket
import Random
import Request.Document
import String.Extra
import StyleSheet exposing (..)
import Task
import Time exposing (Time, second)
import Types exposing (..)
import Update.Auth
import Update.Document
import Update.Page
import Update.Search
import Update.User
import User.Display
import User.Login
import User.Request
import User.Synchronize
import Utility
import Views.Admin exposing (admin)
import Views.Common as Common
import Views.Editor exposing (editor)
import Views.External exposing (windowData, windowSetup)
import Views.Footer as Footer
import Views.Home exposing (home)
import Views.Login exposing (loginPage)
import Views.NavBar as NavBar
import Views.Reader exposing (reader)
import Views.TOC as TOC exposing (toggleListView)
import Views.UserHomePages exposing (userHomePages)
import Views.UserPreferences exposing (userPreferences)
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

        PageMsg submessage ->
            Update.Page.update submessage model

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

        UpdateTextInputBuffer str ->
            ( { model | textInputBuffer = str }, Cmd.none )

        DoSearch searchDomain key ->
            Action.Search.doSearch searchDomain key model

        -- Document.Search.withParameters searchTerm Alphabetical Public ReaderPage model
        MigrateFromAsciidocLatex ->
            Action.Document.migrateFromAsciidocLatex model

        Email email ->
            User.Login.updateEmail model email

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

        ImageSelected ->
            ( { model | message = "Image selected" }
            , External.fileSelected model.imageRecord.id
            )

        ImageRead data ->
            let
                newImage =
                    { contents = data.contents
                    , filename = data.filename
                    }

                newImageRecord =
                    { id = "ImageInputId", mImage = Just newImage }
            in
            ( { model | imageRecord = newImageRecord }
            , Cmd.none
            )

        GetUploadCredentials ->
            Image.Upload.getUploadCredentials model

        CredentialsResult (Ok result) ->
            Image.Upload.request result.credentials model

        CredentialsResult (Err error) ->
            ( model, Cmd.none )

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

        Tick time ->
            let
                tick =
                    model.tick + 1

                newModel =
                    { model | tick = tick }
            in
            if
                model.appState.page
                    == EditorPage
                    && model.appState.textBufferDirty
                    && model.current_document.attributes.docType
                    /= "master"
            then
                if model.current_document.attributes.textType == "latex" then
                    saveCurrentDocument "" newModel
                else
                    updateCurrentDocumentWithContent newModel
            else if model.appState.online then
                Action.Periodic.do newModel time
            else
                Action.Channel.joinChannel newModel

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

        RequestDate ->
            ( model, Task.perform ReceiveDate Date.now )

        ReceiveDate date ->
            let
                nextModel =
                    { model | date = Just date }
            in
            ( nextModel, Cmd.none )

        RequestTime ->
            ( model, Task.perform ReceiveTime Time.now )

        ReceiveTime time ->
            let
                time_ =
                    Just time

                token =
                    model.current_user.token

                ( expired, message ) =
                    if token /= "" then
                        case Jwt.isExpired time token of
                            Ok False ->
                                ( False, "Session valid" )

                            Ok True ->
                                ( True, "Session expired" )

                            Err error ->
                                ( True, "Session expired (2)" )
                    else
                        ( False, "Not signed in" )

                ( newModel, cmd ) =
                    if expired then
                        User.Login.signout "You are now signed out." model
                    else
                        ( model, Cmd.none )

                _ =
                    Debug.log "TOK" message
            in
            ( { newModel | message = message, time = time_ }, cmd )

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
        [ Time.every (model.appState.tickInterval * Time.second) Tick
        , Window.resizes (\{ width, height } -> Resize width height)
        , External.reconnectUser (UserMsg << ReconnectUser)
        , External.recoverUserState (UserMsg << RecoverUserState)
        , Phoenix.Socket.listen model.phxSocket PhoenixMsg
        , External.getRenderedText (DocMsg << GetRenderedText) -- pull rendered text from JS-land, then store in DB
        , External.fileContentRead ImageRead
        , fileUploaded FileUploaded
        ]


page : Model -> List (Element Styles variation Msg)
page model =
    case model.appState.page of
        ReaderPage ->
            reader model

        PublicPage _ ->
            reader model

        PrivatePage _ ->
            reader model

        EditorPage ->
            editor model

        ImagePage ->
            Image.View.imageEditor model

        StartPage ->
            home model

        LoginPage ->
            loginPage model

        AdminPage ->
            admin model

        UserHomePages ->
            userHomePages model

        UserPreferencesPage ->
            userPreferences model


view : Model -> Html Msg
view model =
    EL.root StyleSheet.stylesheet <|
        column None
            []
            [ NavBar.navigation model
            , hairline Hairline
            , el None [ center, EA.width (percent 100) ] <|
                column Main
                    [ spacing 0 ]
                    (List.concat
                        [ page model ]
                    )
            , screen (Footer.footer model)
            ]



-- INIT


init : Flags -> Navigation.Location -> ( Model, Cmd Msg )
init flags location =
    let
        _ =
            Debug.log "On startup, location.href" location.href

        loc =
            String.Extra.replace "#@" "##" location.href

        _ =
            Debug.log "On startup, loc" loc

        maybeId =
            Parser.run Url.id loc

        _ =
            Debug.log "On startup, maybeId" maybeId

        id =
            case maybeId of
                Result.Ok id ->
                    id

                Err error ->
                    0

        current_user =
            { name = ""
            , username = ""
            , id = 0
            , email = ""
            , password = ""
            , blurb = ""
            , token = ""
            , admin = False
            }

        title =
            "Test document"

        content =
            "Welcome"

        rendered_content =
            "Welcome"

        searchState =
            SearchState "" Public Viewed

        ws =
            windowSetup 150 50 StartPage False False

        appState =
            { activeDocumentList = SearchResultList
            , online = False
            , deleteState = Resting
            , signedIn = False
            , authorizing = False
            , registerUser = False
            , menuDropped = False
            , textTypeMenuDropped = False
            , docTypeMenuDropped = False
            , textBufferDirty = False
            , masterDocLoaded = False
            , masterDocOpened = False
            , seed = 0
            , tickerPaused = False
            , page = StartPage
            , tool = TableOfContents
            , editRecord = emptyEditRecord
            , tickInterval = Configuration.tickInterval
            , command = ""
            }

        channel =
            Phoenix.Channel.init "room:lobby"

        ( initSocket, phxCmd ) =
            Phoenix.Socket.init Configuration.websocketHost
                |> Phoenix.Socket.withDebug
                |> Phoenix.Socket.on "shout" "room:lobby" ReceiveChatMessage
                |> Phoenix.Socket.join channel

        emptyUserStateRecord =
            { documentIntStack = [], currentDocumentId = Err "not defined", token = "" }

        model =
            { window = KWindow flags.width flags.height
            , device = Common.getDevice flags.width
            , counter = 0
            , appState = appState
            , message = "Please sign in"
            , errorMsg = ""
            , searchQueryInputBuffer = ""
            , textInputBuffer = ""
            , warning = ""
            , current_user = current_user
            , current_document = Document.startDocument
            , specialDocument = Document.emptyDocument
            , master_document = Document.defaultMasterDocument
            , documents = [ Document.defaultDocument ]
            , documents2 = []
            , documentKey = "blurb"
            , documentDict = Dict.empty
            , documentStack = []
            , searchState = searchState
            , phxSocket = initSocket
            , messageInProgress = ""
            , messages = []
            , imageRecord = defaultImageRecord
            , fileInputId = ""
            , date = Nothing
            , time = Nothing
            , tick = 0
            , lastEditTime = Nothing
            , fileToUpload = Nothing
            , userList = []
            , userStateRecord = emptyUserStateRecord
            , selectedUserName = ""
            }

        standardCommands =
            [ Cmd.map PhoenixMsg phxCmd
            , toJs ws
            , External.askToReconnectUser "reconnectUser"
            , External.askToRecoverUserState "recoverUserState"
            , Task.perform ReceiveDate Date.now
            , Task.perform ReceiveTime Time.now
            , Document.Dictionary.setPublicItemInDict "ident=2017-8-26@18-1-42.887330" "welcome"
            , Random.generate NewSeed (Random.int 1 10000)
            ]

        masterDocumentCommands =
            [ Navigation.newUrl (Configuration.client ++ "/##public/" ++ toString id) ]

        -- ( newModel, command ) =
        --     Document.Search.getRandomDocuments model
        startupPageCommands =
            [ Request.Document.getDocumentWithQuery (DocMsg << GetSpecialDocument) "ident=2017-8-26@18-1-42.887330"
            ]

        commands =
            if id > 0 then
                standardCommands ++ masterDocumentCommands
            else
                startupPageCommands ++ standardCommands
    in
    ( model, Cmd.batch commands )



--  {"width":1218,"height":686,"page":"StartPage","online":true,"signed_in":false}
