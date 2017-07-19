module Main exposing (..)

import Html exposing (..)
import Phoenix.Socket
import Phoenix.Channel


-- begin style

import StyleSheet exposing (..)
import Element as EL exposing (..)
import Element.Attributes as EA exposing (..)
import Window exposing (..)
import Types exposing (..)
import Action.User exposing (..)
import Action.Search exposing (..)
import Action.Document
    exposing
        ( createDocument
        , updateDocuments
        , selectDocument
        , selectNewDocument
        , updateCurrentDocument
        , togglePublic
        , updateCurrentDocumentWithContent
        , updateTags
        , saveCurrentDocument
        )
import Data.Document exposing (documents)
import Data.User exposing (userRecord)
import Request.User exposing (loginUserCmd, getTokenCompleted, registerUserCmd)
import Request.Document exposing (getDocumentsWith)
import Request.Api exposing (loginUrl, registerUserUrl)
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
import Json.Decode as JsDecode
import Action.Channel


-- new style

import Views.Component as Component
import Views.Home exposing (home)
import Views.Reader exposing (reader)
import Views.Editor exposing (editor)
import Views.Image exposing (imageEditor)
import Utility


main : Program Flags Model Msg
main =
    programWithFlags
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
            Action.UI.goToPage p model

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
                Action.User.signout "You are now signed out." model
            else
                Action.UI.setAuthorizing model True

        CancelAuthentication ->
            Action.UI.toggleAuthorizing model

        Login ->
            ( Action.User.login model, loginUserCmd model loginUrl )

        ReconnectUser jsonString ->
            doReconnectUser jsonString model

        Register ->
            ( model, registerUserCmd model registerUserUrl )

        GetTokenCompleted result ->
            getTokenCompleted model result

        Signout ->
            signout "Please sign in" model

        ToggleRegister ->
            toggleRegister model

        ToggleMenu menu ->
            toggleMenu menu model

        SetSearchTerm searchTerms ->
            updateSearch model searchTerms

        -- updatedSearchState
        DoSearch searchDomain key ->
            Action.Document.doSearch searchDomain key model

        RecallLastSearch ->
            ( { model | documents = model.documents2 }, Cmd.none )

        DoRender key ->
            Action.Document.renderDocumentWithKey key model

        GetDocuments (Ok serverReply) ->
            case (Data.Document.documents serverReply) of
                Ok documentsRecord ->
                    updateDocuments model documentsRecord

                Err error ->
                    ( { model | info = (toString error) }
                    , Cmd.none
                    )

        GetDocuments (Err _) ->
            ( { model | info = "Error on GET: " ++ (toString Err) }, Cmd.none )

        GetUserDocuments (Ok documentsRecord) ->
            updateDocuments model documentsRecord

        GetUserDocuments (Err error) ->
            ( { model | message = "Error: could not get user documents." }, Cmd.none )

        -- Action.User.signout "Error: could not get user documents." model
        -- ( { model | message = "Error, cannot get documents" }, Cmd.none )
        PutDocument (Ok serverReply) ->
            case (serverReply) of
                () ->
                    ( model, Cmd.none )

        PutDocument (Err _) ->
            ( { model | info = "Error on PUT: " ++ (toString Err) }, Cmd.none )

        NewDocument ->
            let
                newDocument =
                    Document 0 "abcd" 0 "New Document" "New Content" "New Content" defaultAttributes [] []
            in
                createDocument model newDocument

        --( model , Request.Document.createDocument newDocument model.current_user.token )
        CreateDocument (Ok documentRecord) ->
            selectNewDocument model documentRecord.document

        CreateDocument (Err errorMessage) ->
            ( { model | info = (toString errorMessage) }, Cmd.none )

        DeleteCurrentDocument ->
            ( { model | message = "Delete current document" }, Request.Document.deleteCurrentDocument model )

        DeleteDocument (Ok serverReply) ->
            let
                documents =
                    model.documents

                updatedDocuments =
                    Utility.removeWhen (\doc -> doc.id == model.current_document.id) documents

                newCurrentDocument =
                    (List.head updatedDocuments) |> Maybe.withDefault Types.defaultDocument
            in
                ( { model
                    | message = "Document deleted, remaining = " ++ (toString (List.length updatedDocuments))
                    , documents = updatedDocuments
                    , current_document = newCurrentDocument
                  }
                , Cmd.none
                )

        -- getDocumentsWith newSearchState model.current_user.token
        DeleteDocument (Err errorMessage) ->
            ( { model | info = (toString errorMessage) }, Cmd.none )

        Title title ->
            let
                doc =
                    model.current_document

                new_document =
                    { doc | title = title }
            in
                updateCurrentDocument model new_document

        SetTextType textType ->
            Action.Document.setTextType textType model

        SetDocType docType ->
            Action.Document.setDocType docType model

        InputTags tagString ->
            updateTags tagString model

        SaveCurrentDocument ->
            saveCurrentDocument model

        -- ( { model | current_document = new_document, message = "Title = " ++ new_document.title }, Cmd.none )
        SelectDocument document ->
            selectDocument model document

        SelectMaster document ->
            Action.Document.selectMasterDocument document model

        InputContent content ->
            let
                appState =
                    model.appState

                newAppState =
                    { appState | textBuffer = content, textBufferDirty = True }
            in
                ( { model | appState = newAppState }, Cmd.none )

        {-
           Rationalize: (1) Refresh (2) DoRender (3) InputContent, (3) Title
        -}
        Refresh ->
            ( { model | message = "Refresh, rendering" }, Action.Document.renderDocument model.current_document )

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
                Action.Channel.sendImmediateMessage "hello" model
            else
                Action.Channel.joinChannel model

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



-- app.js:7580 Phoenix message: { event = "phx_reply",
--   topic = "room:lobby",
--   payload = { status = "ok",
--   response = { message = "hello" } },
--   ref = Just 27
-- }
--
-- app.js:7580 PhoenixMsg: NoOp
-- https://github.com/fbonetti/elm-phoenix-socket/issues/29


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every (1 * Time.second) Tick
        , Window.resizes (\{ width, height } -> Resize width height)
        , External.reconnectUser ReconnectUser
        , Phoenix.Socket.listen model.phxSocket PhoenixMsg
        , External.fileContentRead ImageRead
        , fileUploaded FileUploaded
        ]


page : Model -> List (Element Styles variation Msg)
page model =
    case model.appState.page of
        ReaderPage ->
            reader model

        EditorPage ->
            editor model

        ImagePage ->
            imageEditor model

        HomePage ->
            home model


view : Model -> Html Msg
view model =
    EL.root StyleSheet.stylesheet <|
        column None
            []
            [ Component.navigation model
            , hairline Hairline
            , el None [ center, EA.width (percent 100) ] <|
                column Main
                    [ spacing 50 ]
                    (List.concat
                        [ page model
                        ]
                    )
            ]



-- INIT


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        current_user =
            User "" "" "" "" ""

        title =
            "Test document"

        content =
            "Welcome"

        rendered_content =
            "Welcome"

        doc =
            Document 0 "abcd" 0 title content rendered_content defaultAttributes [] []

        searchState =
            SearchState "" Public

        ws =
            windowSetup 150 50 HomePage False False

        appState =
            AppState False False False False False False False False False HomePage TableOfContents ""

        channel =
            Phoenix.Channel.init "room:lobby"

        ( initSocket, phxCmd ) =
            Phoenix.Socket.init "ws://localhost:4000/socket/websocket"
                |> Phoenix.Socket.withDebug
                |> Phoenix.Socket.on "shout" "room:lobby" ReceiveChatMessage
                |> Phoenix.Socket.join channel
    in
        ( Model
            (KWindow flags.width flags.height)
            0
            appState
            "Please sign in"
            current_user
            ""
            ""
            doc
            [ doc ]
            []
            searchState
            initSocket
            ""
            []
            defaultImageRecord
            ""
        , Cmd.batch [ Cmd.map PhoenixMsg phxCmd, toJs ws, External.askToReconnectUser "reconnectUser", Action.Document.renderDocument doc ]
        )



--  {"width":1218,"height":686,"page":"HomePage","online":true,"signed_in":false}
