module Main exposing (..)

import Navigation
import Html exposing (..)
import Phoenix.Socket
import Phoenix.Channel
import Task
import Nav.Parser exposing (..)
import Nav.Navigation
import Date exposing(Date)
import Configuration
import Dict
import Request.Document
import Action.Error
import Document.RenderAsciidoc
import Document.Search
-- import UrlParser as Url exposing ((</>), (<?>), s, int, stringParam, top)
import Parser
import Nav.UrlParseExtra as Url


-- begin style

import StyleSheet exposing (..)
import Element as EL exposing (..)
import Element.Attributes as EA exposing (..)
import Window exposing (..)
import Types exposing (..)
import Views.Component as Component
import User.Login exposing (..)
import Action.Search exposing (..)
import Action.Page
import Image.Upload
import Action.Document
    exposing
        ( createDocument
        , updateDocuments
        , selectDocument
        , selectNewDocument
        , updateCurrentDocument
        , togglePublic
        , toggleUpdateRate
        , updateCurrentDocumentWithContent
        , updateTags
        , saveCurrentDocument
        , deleteDocument
        )
import Data.Document exposing (documents)
import User.Auth exposing (loginUserCmd, getTokenCompleted, registerUserCmd)
import Request.Document exposing (getDocumentsWith)
import Request.Api exposing (loginUrl, registerUserUrl)
import User.Request
import Time exposing (Time, second)
import Views.External exposing (windowData, windowSetup)
import External exposing (render, toJs, fileUpload, fileUploaded)
import Request.Document
import Action.UI
    exposing
        ( displayPage
        , toggleMenu
        , toggleRegister
        , updateToolStatus
        , appStateWithPage
        , toggleAuthorizing
        , appStateToggleAuthorizing
        )
import Phoenix.Socket
import Action.Channel
import User.Display
import Jwt


-- new style

import Views.Component as Component
import Views.Home exposing (home)
import Views.Reader exposing (reader)
import Views.Editor exposing (editor)
import Image.View
import Views.Admin exposing (admin)
import Views.UserHomePages exposing (userHomePages)


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
    in
        { model | window = new_window, message = "w: " ++ (toString model.window.width) ++ ", h: " ++ (toString model.window.height) }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( { model | message = "NoOp" }, Cmd.none )

        Resize w h ->
            ( (updateWindow model w h), toJs (Views.External.windowData model model.appState.page) )

        GoTo p ->
            Action.Page.goToPage p model

        SelectTool t ->
            ( { model | appState = (updateToolStatus model t) }, Cmd.none )

        Name name ->
            updateName model name

        Username username ->
            updateUsername model username

        Email email ->
            updateEmail model email

        Password password ->
            updatePassword model password

        AuthenticationAction ->
            if model.appState.signedIn then
                User.Login.signout "You are now signed out." model
            else
                Action.UI.setAuthorizing model True

        CancelAuthentication ->
            Action.UI.toggleAuthorizing model

        Login ->
          let
            (model1, cmds1) = User.Login.login2 model
            (model2, cmds2) = if model1.appState.signedIn then
                (model1, Cmd.none) --Action.Document.search Private "sort=viewed&limit=12" ReaderPage model1
            else
              (model1, Cmd.none)
          in
            (model2, Cmd.batch [cmds1, cmds2])

        ReconnectUser jsonString ->
            doReconnectUser jsonString model

        Register ->
          let
            _ = Debug.log "REGISTER" 1
          in
            ( model, User.Auth.registerUserCmd model Request.Api.registerUserUrl )

        CompleteRegistration result ->
          case (Debug.log "CompleteRegistration" result) of
            Ok result ->
              let
                newUser = {
                  name =  result.name
                , username = result.username
                , id = 0
                , email = result.email
                , password = ""
                , blurb = ""
                , token = result.token
                , admin = False
              }
                oldAppState = model.appState
                newAppState = {oldAppState | signedIn = True, authorizing = False}
              in
                ({ model | current_user = newUser, appState = newAppState}, Cmd.none)
            Err err ->
                ({model | message = Action.Error.httpErrorString err}, Cmd.none)

        GetTokenCompleted result ->
            User.Auth.getTokenCompleted model result

        Signout ->
            signout "Please sign in" model

        ToggleRegister ->
            toggleRegister model

        ToggleUpdateRate ->
           (Action.Document.toggleUpdateRate model , Cmd.none)

        ToggleMenu menu ->
            toggleMenu menu model

        SetSearchTerm searchTerms ->
            updateSearch model searchTerms

        UpdateTextInputBuffer str ->
            ({model | textInputBuffer = str} , Cmd.none)

        SelectSearchMode searchMode ->
          let
             domain = case searchMode of
                "private" -> Private
                "public" -> Public
                "all" -> All
                _ -> Public
             oldSearchState = model.searchState
             newSearchState = if model.appState.signedIn then
               { oldSearchState  | domain = domain }
             else
               { oldSearchState  | domain = Public }
          in
            ({ model | searchState = newSearchState }, Cmd.none  )

        SelectSearchOrder searchOrder ->
          let
             order = case searchOrder of
                "viewed" -> Viewed
                "created" -> Created
                "alpha" -> Alphabetical
                _ -> Viewed
             oldSearchState = model.searchState
             newSearchState = { oldSearchState  | order = order }
          in
            ({ model | searchState = newSearchState }, Cmd.none  )

        ClearSearch ->
          ({model | textInputBuffer = ""}, Cmd.none)
            -- updateSearch model ""

        -- updatedSearchState
        DoSearch searchDomain key ->
          let
            searchState = model.searchState
            newSearchState = { searchState | query = model.textInputBuffer }
            newModel = { model | searchState = newSearchState }
          in
            Action.Document.searchOnEnter searchDomain key newModel

        RecallLastSearch ->
           Action.Document.recallLastSearch model

        UserHomePage ->
          let
            searchTerm = "key=home&authorname=" ++ (User.Login.shortUsername model)
          in
            Document.Search.withParameters searchTerm Alphabetical Public ReaderPage model

        GetPublicPage searchTerm ->
            Document.Search.withParameters searchTerm Alphabetical Public ReaderPage model

        InitHomePage ->
          Document.Search.withParameters "sort=viewed&limit=25" Viewed Private HomePage model

        DoRender key ->
            Document.RenderAsciidoc.putWithKey key model


        GetRenderedText str ->
          let
            document = model.current_document
            newDocument = { document | rendered_content = str }
            newModel = { model | current_document = newDocument }
          in
            Action.Document.saveCurrentDocument "" newModel
            -- ({model | current_document = newDocument}, Cmd.none)
            -- Request.Document.putDocument "" model newDocument

        GotoUserHomePages ->
          User.Display.goToUserHomePages model

        SearchForUserHomePages keyCode ->
          if keyCode == 13 then
            let
              _ = Debug.log "SearchForUserHomePages, keyCode" keyCode
              query = "is_user=" ++ model.textInputBuffer
            in
              ( model, User.Request.getList query )
          else
            (model, Cmd.none)


        GetHomePageForUserHomePages searchTerm username->
          let
            model2 = { model | selectedUserName = username}
            (newModel, cmd) = Document.Search.withParameters searchTerm Alphabetical Public UserHomePages model2
          in
            ( newModel, Cmd.batch[ cmd] )

        GetUsers (Ok usersRecord) ->
          let
            _ = Debug.log "Ok, updating" "USERS (123)"
            userList = usersRecord.users
            user = List.head userList |> Maybe.withDefault model.current_user
            query = "authorname=" ++ user.username ++ "&key=home"
            (model1, cmd) = Document.Search.withParameters query Alphabetical Public UserHomePages model
          in
            ({model1 | userList = userList, selectedUserName = user.username}, cmd)

        GetUsers (Err error) ->
          let
             _ = Debug.log "ERROR, usersRecord" error
          in
            ({model | message = Action.Error.httpErrorString error}, Cmd.none)

        GetDocuments (Ok serverReply) ->
            case (Data.Document.documents serverReply) of
                Ok documentsRecord ->
                    updateDocuments model documentsRecord

                Err error ->
                    ( { model | message = "Error in GetDocuments" }
                    , Cmd.none
                    )

        GetDocuments (Err error) ->
            ( { model | message = Action.Error.httpErrorString error }, Cmd.none )

        GetUserDocuments (Ok documentsRecord) ->
            updateDocuments model documentsRecord


        GetUserDocuments (Err error) ->
            ( { model | message = Action.Error.httpErrorString error }, Cmd.none )


        GetSpecialDocument (Ok documentsRecord) ->
           let
               specialDocument =
                   case List.head documentsRecord.documents of
                       Just document ->
                           document

                       Nothing ->
                           defaultDocument
            in
               ({model | specialDocument = specialDocument } , Cmd.none)

        GetSpecialDocument (Err err) ->
            ({model | message = "Getting special document: error" } , Cmd.none)

        -- User.Login.signout "Error: could not get user documents." model
        -- ( { model | message = "Error, cannot get documents" }, Cmd.none )
        PutDocument (Ok serverReply) ->
            case (serverReply) of
                () ->
                    ( model, Cmd.none )

        PutDocument (Err error) ->
            ( { model | message = Action.Error.httpErrorString error }, Cmd.none )

        NewDocument ->
            let
                newDocument =
                    Document 0 "abcd" 0 "New Document" "New Content" "New Content" defaultAttributes [] [] 0 ""
            in
                createDocument model newDocument

        AddToMasterDocument ->
           Action.Document.addToMasterDocument model
        --( model , Request.Document.createDocument newDocument model.current_user.token )

        AttachCurrentDocument location ->
          let
            appState = model.appState
            newAppState = { appState | command = (Action.Document.attachDocumentCommand location model)}
          in
          ({model | appState = newAppState}, Cmd.none)

        CreateDocument (Ok documentRecord) ->
            selectNewDocument model documentRecord.document

        CreateDocument (Err error) ->
            ( { model | message = Action.Error.httpErrorString error }, Cmd.none )

        DeleteCurrentDocument ->
            ( { model | message = "Delete current document" }, Request.Document.deleteCurrentDocument model )

        DeleteDocument serverReply ->
          Action.Document.deleteDocument serverReply model

        Title title ->
            Action.Document.setTitle title model

        SetTextType textType ->
            Action.Document.setTextType textType model

        SetDocType docType ->
            Action.Document.setDocType docType model

        SetParentId parentIdString ->
            Action.Document.setParentId parentIdString model

        InputTags tagString ->
            updateTags tagString model

        SaveCurrentDocument ->
            saveCurrentDocument "" model

        AdoptChildren ->
            saveCurrentDocument "adopt_children=yes" model

        SelectDocument document ->
          -- XXX: when a document is selected update "viewed_at"
          -- XXX: do we restrict this to views by user?
          let
            _ = Debug.log "SelectDocument, id" document.id
            _ = Debug.log "document.author_id" document.author_id

            (model1, cmd1) = if document.author_id == model.current_user.id then
                Action.Document.saveDocument "viewed_at=now" document model
              else
                (model, Cmd.none)
            (model2, cmd2) = selectDocument model document
          in
            (model2, Cmd.batch[cmd1, cmd2])


        SelectMaster document ->
            Action.Document.selectMasterDocument document model

        InputContent content ->
            Action.Document.inputContent content model
        {-
           Rationalize: (1) Refresh (2) DoRender (3) InputContent, (3) Title
        -}
        Refresh ->
          updateCurrentDocumentWithContent model.appState.textBuffer model

        UseSearchDomain searchDomain ->
            updateSearchDomain model searchDomain

        TogglePublic ->
            togglePublic model

        ImageSelected ->
            ( {model | message = "Image selected"}
            , External.fileSelected model.imageRecord.id
            )

        ImageRead data ->
            let
                newImage =
                    { contents = Debug.log "IMAGE CONTENTS" data.contents
                    , filename = data.filename
                    }
                newImageRecord = {id = "ImageInputId", mImage = Just newImage }
            in
                ( { model | imageRecord = newImageRecord }
                , Cmd.none
                )

        GetUploadCredentials ->
          Image.Upload.getUploadCredentials model

        CredentialsResult (Ok result) ->
            Image.Upload.request result.credentials model


        CredentialsResult (Err error) ->
          let
            _ = Debug.log "error" error
          in
            (model, Cmd.none)


        Files nativeFiles ->
          ( {model | fileToUpload = List.head nativeFiles }, Cmd.none)


        -----

        UploadComplete (Ok result) ->
          let
            _ = Debug.log "ok" result
          in
            (model, Cmd.none)

        UploadComplete (Err error) ->
          let
            _ = Debug.log "error" error
          in
            (model, Cmd.none)

        FileSelected ->
            ( model, fileUpload model.fileInputId )

        FileUploaded True ->
            -- obviously, set some state notifying success
            ( model, Cmd.none )

        FileUploaded False ->
            -- obviously, set some state notifying failure
            ( model, Cmd.none )

        Tick time ->
            if model.appState.page == EditorPage && model.appState.textBufferDirty then
                updateCurrentDocumentWithContent model.appState.textBuffer model
            else if model.appState.online then
                Action.Channel.sendImmediateMessage "hello" model  -- (model, Cmd.none) --
            else
                Action.Channel.joinChannel model  -- (model, Cmd.none) --

        SendToJS str ->
            ( model, toJs str )

        SetupPages ->
            ( model, toJs (Views.External.windowData model model.appState.page) )

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

        GoToPage maybepage ->
           Nav.Navigation.navigateTo maybepage  model

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
                time_ = Just time
                message = case Jwt.isExpired time model.current_user.token of
                  Ok val ->
                    "token is valid"
                  Err error ->
                    "token is expired"
                nextModel =
                    { model | time = time_, message = message }
            in
                ( nextModel, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every ((model.appState.tickInterval) * Time.second) Tick
        , Window.resizes (\{ width, height } -> Resize width height)
        , External.reconnectUser ReconnectUser
        , Phoenix.Socket.listen model.phxSocket PhoenixMsg
        , External.getRenderedText GetRenderedText -- pull rendered text from JS-land, then store in DB
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

        HomePage ->
            home model

        AdminPage ->
            admin model

        UserHomePages ->
            userHomePages model


view : Model -> Html Msg
view model =
    EL.root StyleSheet.stylesheet <|
        column None
            []
            [ Component.navigation model
            , hairline Hairline
            , el None [ center, EA.width (percent 100) ] <|
                column Main
                    [ spacing 0 ]
                    (List.concat
                        [ page model ]
                    )
            , screen  (Component.footer model)
            ]



-- INIT


init : Flags -> Navigation.Location -> ( Model, Cmd Msg )
init flags location =
    let
        _ = Debug.log "LOCATION" location.href

        maybeId = Parser.run Url.id location.href

        id = case maybeId of
               Result.Ok id -> id
               Err error -> 0

        _ = Debug.log "ID" id

        -- location = "localhost:3000/##public/" ++ (toString id)

        current_user =
          {
          name = ""
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
            windowSetup 150 50 HomePage False False

        appState = {
         online = False
         , signedIn = False
         , authorizing = False
         , registerUser =  False
         , menuDropped = False
         , textTypeMenuDropped = False
         , docTypeMenuDropped = False
         , textBufferDirty = False
         , masterDocLoaded = False
         , tickerPaused = False
         , page = HomePage
         , tool = TableOfContents
         , textBuffer = ""
         , tickInterval = Configuration.tickInterval
         , command = ""
       }

        channel =
            Phoenix.Channel.init "room:lobby"

        ( initSocket, phxCmd ) =
            Phoenix.Socket.init "ws://localhost:4000/socket/websocket"
                |> Phoenix.Socket.withDebug
                |> Phoenix.Socket.on "shout" "room:lobby" ReceiveChatMessage
                |> Phoenix.Socket.join channel

        model = {
            window = (KWindow flags.width flags.height)
            , counter = 0
            , appState = appState
            , message = "Please sign in"
            , errorMsg = ""
            , textInputBuffer = ""
            , warning = ""
            , current_user = current_user
            , current_document = startDocument
            , specialDocument = defaultDocument
            , master_document = defaultMasterDocument
            , documents = [ defaultDocument ]
            , documents2 = []
            , documentKey = "blurb"
            , documentDict = Dict.empty
            , documentStack = []
            , searchState = searchState
            , phxSocket = initSocket
            , messageInProgress = ""
            , messages =[]
            , imageRecord = defaultImageRecord
            , fileInputId = ""
            , date = Nothing
            , time = Nothing
            , fileToUpload = Nothing
            , userList = []
            , selectedUserName = ""
          }

        standardCommands = [
          Cmd.map PhoenixMsg phxCmd, toJs ws
        , External.askToReconnectUser "reconnectUser"
        , Task.perform ReceiveDate Date.now
        , Task.perform ReceiveTime Time.now
        ]

        masterDocumentCommands = [ Navigation.newUrl (Configuration.client ++ "/##public/" ++ (toString id)) ]

        startupPageCommands = [
          (Action.Document.search Private "sort=updated&limit=12" HomePage model |> Tuple.second)
           , Request.Document.getSpecialDocumentWithQuery "ident=2017-8-4@22-21-10.03ed17"
        ]

        commands = if id > 0 then
            standardCommands ++ masterDocumentCommands
          else
            standardCommands ++ startupPageCommands

    in
        ( model , Cmd.batch commands )



--  {"width":1218,"height":686,"page":"HomePage","online":true,"signed_in":false}
