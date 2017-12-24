module Init exposing (init)

import Configuration
import Date
import Dict
import Document.Dictionary
import Document.Document as Document
import External
import MiniLatex.Differ exposing (EditRecord, emptyEditRecord)
import Nav.UrlParseExtra as Url
import Navigation
import Parser
import Phoenix.Channel
import Phoenix.Socket
import Random
import Request.Document
import String.Extra
import Task
import Time
import Types
    exposing
        ( ActiveDocumentList(..)
        , DeleteState(..)
        , DocMsg(..)
        , Flags
        , ImageRecord
        , KWindow
        , Model
        , Msg
            ( DocMsg
            , NewSeed
            , PhoenixMsg
            , ReceiveChatMessage
            , ReceiveDate
            , ReceiveTime
            )
        , Page(..)
        , SearchDomain(..)
        , SearchOrder(..)
        , SearchState
        , Tool(..)
        , defaultImageRecord
        )
import Views.Common as Common
import Views.External


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
            Views.External.windowSetup 150 50 StartPage False False

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
            , External.toJs ws
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
