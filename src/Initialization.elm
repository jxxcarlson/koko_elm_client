module Initialization exposing (..)

import Types exposing (..)
import Document.Document as Document
import Configuration
import Dict
import MiniLatex.Differ exposing (EditRecord, emptyEditRecord)
import Phoenix.Socket


nullUser : User
nullUser =
    { name = ""
    , username = ""
    , id = 0
    , email = ""
    , password = ""
    , blurb = ""
    , token = ""
    , admin = False
    }


resetAppState : Model -> AppState
resetAppState model =
    let
        appState =
            model.appState
    in
        { appState
            | activeDocumentList = SearchResultList
            , online = False
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


resetModel : Model -> Model
resetModel model =
    let
        newAppState =
            resetAppState model
    in
        { model
            | counter = 0
            , appState = newAppState
            , message = "Please sign in"
            , errorMsg = ""
            , searchQueryInputBuffer = ""
            , textInputBuffer = ""
            , warning = ""
            , current_user = nullUser
            , current_document = Document.startDocument
            , specialDocument = Document.emptyDocument
            , master_document = Document.defaultMasterDocument
            , documents = [ Document.defaultDocument ]
            , documents2 = []
            , documentKey = "blurb"
            , documentDict = Dict.empty
            , documentStack = []
            , searchState = SearchState "" Public Viewed

            -- , phxSocket = initSocket
            , messageInProgress = ""
            , messages = []
            , imageRecord = defaultImageRecord
            , fileInputId = ""
            , date = Nothing
            , time = Nothing
            , fileToUpload = Nothing
            , userList = []
            , selectedUserName = ""
        }
