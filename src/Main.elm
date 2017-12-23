module Main exposing (..)

-- 1
-- 2

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
import Action.Page
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
import MiniLatex.LatexState exposing (emptyLatexState)
import Nav.Navigation
import Nav.Parser exposing (..)
import Nav.UrlParseExtra as Url
import Navigation
import Parser
import Phoenix.Channel
import Phoenix.Socket
import Random
import Request.Api exposing (loginUrl, registerUserUrl)
import Request.Document
import String.Extra
import StyleSheet exposing (..)
import Task
import Time exposing (Time, second)
import Types exposing (..)
import User.Auth exposing (getTokenCompleted, loginUserCmd, registerUserCmd)
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

        AuthMsg Foo ->
            ( { model | message = "DocMsg: Foo" }, Cmd.none )

        AuthMsg (Name name) ->
            User.Login.updateName model name

        AuthMsg (Username username) ->
            User.Login.updateUsername model username

        AuthMsg (Password password) ->
            User.Login.updatePassword model password

        AuthMsg Signout ->
            User.Login.signout "Please sign in" model

        AuthMsg AuthenticationAction ->
            if model.appState.signedIn then
                User.Login.signout "You are now signed out." model
            else
                Action.UI.setAuthorizing model True

        AuthMsg CancelAuthentication ->
            Action.UI.toggleAuthorizing model

        AuthMsg Login ->
            User.Login.doLogin model

        AuthMsg Register ->
            ( model, User.Auth.registerUserCmd model Request.Api.registerUserUrl )

        AuthMsg (CompleteRegistration result) ->
            User.Login.completeRegistration result model

        AuthMsg (GetTokenCompleted result) ->
            User.Auth.getTokenCompleted model result

        AuthMsg SignOutOrIn ->
            User.Login.signOutOrIn model

        ToggleListView ->
            TOC.toggleListView model

        AuthMsg ToggleRegister ->
            toggleRegister model

        ToggleUpdateRate ->
            ( Action.Document.toggleUpdateRate model, Cmd.none )

        ToggleMenu menu ->
            toggleMenu menu model

        SearchMsg (SetSearchTerm searchTerms) ->
            Document.Search.update model searchTerms

        SearchMsg (UpdateSearchQueryInputBuffer str) ->
            ( { model | searchQueryInputBuffer = str }, Cmd.none )

        UpdateTextInputBuffer str ->
            ( { model | textInputBuffer = str }, Cmd.none )

        SearchMsg (SelectSearchMode searchMode) ->
            Action.Search.selectSearchMode searchMode model

        SearchMsg (SelectSearchOrder searchOrder) ->
            Action.Search.selectSearchOrder searchOrder model

        SearchMsg ClearSearch ->
            ( { model | searchQueryInputBuffer = "" }, Cmd.none )

        DoSearch searchDomain key ->
            Action.Search.doSearch searchDomain key model

        SearchMsg RecallLastSearch ->
            Document.Search.recallLastSearch model

        PageMsg UserHomePage ->
            Action.Page.setHomePage model

        -- Document.Search.withParameters searchTerm Alphabetical Public ReaderPage model
        MigrateFromAsciidocLatex ->
            Action.Document.migrateFromAsciidocLatex model

        PageMsg (GetPublicPage searchTerm) ->
            Document.Search.withParameters searchTerm Alphabetical Public ReaderPage model

        PageMsg InitStartPage ->
            let
                appState =
                    model.appState

                newAppState =
                    { appState | page = StartPage, masterDocLoaded = False, authorizing = False }
            in
            ( { model | appState = newAppState }
            , Request.Document.getDocumentWithQuery (DocMsg << GetSpecialDocument) "ident=2017-8-26@18-1-42.887330"
            )

        DocMsg RandomDocuments ->
            Document.Search.getRandomDocuments model

        DocMsg (DoRender key) ->
            Document.Render.putWithKey key model

        DocMsg (GetRenderedText str) ->
            let
                document =
                    model.current_document

                newDocument =
                    { document | rendered_content = str }

                newModel =
                    { model | current_document = newDocument }

                _ =
                    Debug.log "GetRenderedText" "now"
            in
            -- Action.Document.saveCurrentDocument "" newModel
            ( { model | current_document = newDocument }, Cmd.none )

        Email email ->
            User.Login.updateEmail model email

        PageMsg GotoUserHomePages ->
            User.Display.goToUserHomePages model

        PageMsg GotoUserPreferencesPage ->
            let
                appState =
                    model.appState

                newAppState =
                    { appState | page = UserPreferencesPage }
            in
            ( { model
                | appState = newAppState
                , textInputBuffer = model.current_user.blurb
              }
            , User.Request.get model.current_user.id
            )

        SearchMsg (SearchForUserHomePages keyCode) ->
            if keyCode == 13 then
                let
                    query =
                        "is_user=" ++ model.searchQueryInputBuffer

                    searchState =
                        model.searchState

                    newSearchState =
                        { searchState | domain = Public }
                in
                ( { model | searchState = newSearchState }, User.Request.getList query )
            else
                ( model, Cmd.none )

        PageMsg (GetHomePageForUserHomePages searchTerm username) ->
            let
                model2 =
                    { model | selectedUserName = username }

                ( newModel, cmd ) =
                    Document.Search.withParameters searchTerm Alphabetical Public UserHomePages model2
            in
            ( newModel, Cmd.batch [ cmd ] )

        DocMsg EditSpecialDocument ->
            let
                appState =
                    model.appState

                newAppState =
                    { appState | page = EditorPage }
            in
            ( { model | current_document = model.specialDocument, appState = newAppState }
            , Cmd.none
            )

        UserMsg (GetUsers (Ok usersRecord)) ->
            let
                userList =
                    usersRecord.users

                user =
                    List.head userList |> Maybe.withDefault model.current_user

                query =
                    "authorname=" ++ user.username ++ "&key=home"

                ( model1, cmd ) =
                    Document.Search.withParameters query Alphabetical Public UserHomePages model
            in
            ( { model1 | userList = userList, selectedUserName = user.username }, cmd )

        UserMsg (GetUsers (Err error)) ->
            ( { model | message = Action.Error.httpErrorString error }, Cmd.none )

        UserMsg (GetUser (Ok userRecord)) ->
            let
                _ =
                    Debug.log "userRecord" "yo!"

                user =
                    userRecord.user

                current_user =
                    model.current_user

                updatedCurrentUser =
                    { current_user | blurb = user.blurb }
            in
            ( { model | current_user = updatedCurrentUser }, Cmd.none )

        UserMsg (GetUser (Err error)) ->
            let
                _ =
                    Debug.log "error" error
            in
            ( { model | message = Action.Error.httpErrorString error }, Cmd.none )

        UserMsg (GetUserState (Ok userStateRecord)) ->
            let
                _ =
                    Debug.log
                        "in GetUserState"
                        "SUCCESS"

                _ =
                    Debug.log
                        "in GetUserState, userStateRecord"
                        userStateRecord

                appState =
                    model.appState

                searchState =
                    model.searchState

                newSearchState =
                    { searchState | domain = All }

                newAppState =
                    { appState | page = ReaderPage, activeDocumentList = DocumentStackList }

                token =
                    model.current_user.token

                task =
                    User.Synchronize.setUserStateTask userStateRecord token
            in
            ( { model | appState = newAppState, searchState = newSearchState }, Task.attempt SetUserState task )

        UserMsg (GetUserState (Err error)) ->
            let
                _ =
                    Debug.log
                        "in GetUserState ERROR"
                        (toString error)
            in
            ( model, Cmd.none )

        DocMsg (GetDocuments (Ok documentsRecord)) ->
            updateDocuments model documentsRecord

        DocMsg (GetDocuments (Err error)) ->
            ( { model | message = Action.Error.httpErrorString error }, Cmd.none )

        DocMsg (GetUserDocuments (Ok documentsRecord)) ->
            updateDocuments model documentsRecord

        DocMsg (GetUserDocuments (Err error)) ->
            ( { model | message = Action.Error.httpErrorString error }, Cmd.none )

        DocMsg (GetSpecialDocument (Ok documentsRecord)) ->
            let
                specialDocument =
                    case List.head documentsRecord.documents of
                        Just document ->
                            document

                        Nothing ->
                            Document.emptyDocument
            in
            ( { model | specialDocument = specialDocument }, Cmd.none )

        DocMsg (GetSpecialDocument (Err err)) ->
            ( { model | message = "Getting special document: error" }, Cmd.none )

        DocMsg (SetDocumentInDict (Ok ( documentsRecord, key ))) ->
            let
                document =
                    case List.head documentsRecord.documents of
                        Just document ->
                            document

                        Nothing ->
                            Document.emptyDocument

                documentDict =
                    model.documentDict

                newDocumentDict =
                    if document /= Document.emptyDocument then
                        Document.Dictionary.set key document documentDict
                    else
                        documentDict
            in
            ( { model | documentDict = newDocumentDict }, Cmd.none )

        DocMsg (SetDocumentInDict (Err err)) ->
            ( { model | message = "Error setting key in documentDict" }, Cmd.none )

        ---
        DocMsg (GetMasterDocument (Ok documentsRecord)) ->
            let
                masterDocument =
                    case List.head documentsRecord.documents of
                        Just document ->
                            document

                        Nothing ->
                            Document.emptyDocument

                oldDocuments =
                    model.documents

                newDocuments =
                    Utility.replaceIf (Action.Document.hasId masterDocument.id) masterDocument oldDocuments
            in
            ( { model | master_document = masterDocument }, Cmd.none )

        DocMsg (GetMasterDocument (Err err)) ->
            ( { model | message = "Getting master document: error" }, Cmd.none )

        -- User.Login.signout "Error: could not get user documents." model
        -- ( { model | message = "Error, cannot get documents" }, Cmd.none )
        Message str ->
            ( { model | message = str }, Cmd.none )

        DocMsg (PutDocument (Ok serverReply)) ->
            case serverReply of
                () ->
                    ( model, Cmd.none )

        DocMsg (PutDocument (Err error)) ->
            ( { model | message = Action.Error.httpErrorString error }, Cmd.none )

        DocMsg NewDocument ->
            let
                newDocument =
                    Document.defaultDocument
            in
            createDocument model Document.blankDocument

        DocMsg NewDiaryEntry ->
            let
                newDocument =
                    Document.diaryEntry
            in
            createDocument model (Document.diaryEntry model.date)

        DocMsg GetDiary ->
            Document.Search.withParameters "key=diary" Created Private ReaderPage model

        DocMsg AddToMasterDocument ->
            let
                _ =
                    Debug.log "MAIN: AddToMasterDocument" "now"
            in
            Document.MasterDocument.addTo model

        --( model , Request.Document.createDocument newDocument model.current_user.token )
        DocMsg (AttachCurrentDocument location) ->
            let
                appState =
                    model.appState

                newAppState =
                    { appState | command = Document.MasterDocument.attach location model }
            in
            ( { model | appState = newAppState }, Cmd.none )

        DocMsg (CreateDocument (Ok documentRecord)) ->
            selectNewDocument model documentRecord.document

        DocMsg (CreateDocument (Err error)) ->
            ( { model | message = Action.Error.httpErrorString error }, Cmd.none )

        DocMsg RequestDocumentDelete ->
            let
                appState =
                    model.appState

                newAppState =
                    { appState | deleteState = Pending }
            in
            ( { model | appState = newAppState }, Cmd.none )

        DocMsg CancelDocumentDelete ->
            let
                appState =
                    model.appState

                newAppState =
                    { appState | deleteState = Resting }
            in
            ( { model | appState = newAppState }, Cmd.none )

        DocMsg DeleteCurrentDocument ->
            let
                appState =
                    model.appState

                newAppState =
                    { appState | deleteState = Resting }
            in
            ( { model
                | appState = newAppState
                , message = "Delete current document"
              }
            , Request.Document.deleteCurrentDocument model
            )

        DocMsg (DeleteDocument serverReply) ->
            Action.Document.deleteDocument serverReply model

        DocMsg RenumberDocuments ->
            Document.TOC.renumberMasterDocument model

        DocMsg (Title title) ->
            Action.Document.setTitle title model

        DocMsg (SetTextType textType) ->
            Action.Document.setTextType textType model

        DocMsg (SetDocType docType) ->
            Action.Document.setDocType docType model

        DocMsg (SetParentId parentIdString) ->
            Document.MasterDocument.setParentId parentIdString model

        DocMsg (InputTags tagString) ->
            updateTags tagString model

        DocMsg SaveCurrentDocument ->
            let
                _ =
                    Debug.log "SaveCurrentDocument" "now"
            in
            saveCurrentDocument "" model

        DocMsg (SaveDocument result) ->
            ( { model | message = "Document saved" }, Cmd.none )

        DocMsg AdoptChildren ->
            let
                _ =
                    Debug.log "AdoptChildren" "now"
            in
            saveCurrentDocument "adopt_children=yes" model

        DocMsg (SelectDocument document) ->
            let
                _ =
                    Debug.log "SelectDocument" "now"
            in
            Action.Document.selectDocument model document

        DocMsg (SelectMaster document) ->
            Document.MasterDocument.select document model

        DocMsg (InputContent content) ->
            Action.Document.inputContent content model

        DocMsg UpdateDocument ->
            let
                _ =
                    Debug.log "UpdateDocument" "now"
            in
            Action.Document.updateCurrentDocumentWithContent model

        DocMsg LatexFullRender ->
            let
                _ =
                    Debug.log "UpdateDocument" "now"
            in
            Action.Document.latexFullRender model

        DocMsg TogglePublic ->
            togglePublic model

        PageMsg (GoTo p) ->
            Action.Page.goToPage p model

        Resize w h ->
            ( updateWindow model w h, toJs (Views.External.windowData model model.appState.page) )

        SelectTool t ->
            ( { model | appState = updateToolStatus model t }, Cmd.none )

        SearchMsg (UseSearchDomain searchDomain) ->
            Document.Search.updateDomain model searchDomain

        SetUserState (Ok result) ->
            User.Synchronize.setUserState result model

        SetUserState (Err err) ->
            ( { model | message = "Error in SetUserState: " ++ toString err }, Cmd.none )

        UserMsg (ReconnectUser jsonString) ->
            User.Login.doReconnectUser jsonString model

        UserMsg (RecoverUserState jsonString) ->
            User.Synchronize.doRecoverUserState jsonString model

        UserMsg UpdateCurrentUser ->
            Action.User.updateCurrentUser model

        UserMsg (PutUser (Ok serverReply)) ->
            case serverReply of
                () ->
                    ( model, Cmd.none )

        UserMsg (PutUser (Err error)) ->
            ( { model | message = Action.Error.httpErrorString error }, Cmd.none )

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

        PageMsg SetupPages ->
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

        PageMsg (GoToPage maybepage) ->
            Nav.Navigation.navigateTo maybepage model

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
