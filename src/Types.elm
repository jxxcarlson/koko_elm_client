module Types exposing (..)

import Http
import Time exposing (Time)
import Phoenix.Socket
import Json.Encode as JsEncode
import Date exposing (Date)
import Dict
import Image.FileReader as FileReader exposing (NativeFile)
import MiniLatex.Differ exposing (EditRecord)
import Task


type Device
    = Computer
    | Tablet
    | Phone


type alias User =
    { name : String
    , id : Int
    , username : String
    , email : String
    , blurb : String
    , password : String
    , token : String
    , admin : Bool
    }


type alias BigUserRecord =
    { user : User }


type alias Users =
    List User


{-| Use to transfer data to JS-world. Does not contani password
-}
type alias UsersRecord =
    { users : List User
    }


type alias LoginUserRecord =
    { name : String, username : String, id : Int, email : String, token : String, blurb : String }


type alias LoginLocalStorageRecord =
    { name : String, username : String, id : String, email : String, token : String, blurb : String }


type alias UserRecord =
    { user : LoginUserRecord }


type alias UserStateRecord =
    { documentIntStack : List Int
    , currentDocumentId : Result.Result String Int
    , token : String
    }


type alias KWindow =
    { width : Int
    , height : Int
    }


type alias ImagePortData =
    { contents : String
    , filename : String
    }


type alias Image =
    { contents : String
    , filename : String
    }


defaultImage : Image
defaultImage =
    Image "data:image/gif;base64,R0lGODlhAQABAIAAAP///wAAACH5BAEAAAAALAAAAAABAAEAAAICRAEAOw==" "fileName"


type alias ImageRecord =
    { id : String
    , mImage : Maybe Image
    }


defaultImageRecord : ImageRecord
defaultImageRecord =
    ImageRecord "ImageInputId" Nothing


type alias Credentials =
    { signature : String
    , policy : String
    , key : String
    , acl : String
    , awsAccessKeyId : String
    , date : String
    }


type alias CredentialsWrapper =
    { credentials : Credentials
    , url : String
    }


type alias DocumentDict =
    Dict.Dict String Document


type alias DocumentAttributes =
    { public : Bool
    , textType : String
    , docType : String
    , level : Int
    }


type alias DocumentAttributesRecord =
    { attributes : DocumentAttributes }


type alias Child =
    { title : String
    , level : Int
    , doc_identifier : String
    , doc_id : Int
    , comment : String
    }


type alias Document =
    { id : Int
    , identifier : String
    , author_id : Int
    , author_name : String
    , title : String
    , content : String
    , rendered_content : String
    , attributes : DocumentAttributes
    , tags :
        List String
    , children : List Child
    , parent_id : Int
    , parent_title : String
    }


type alias DocumentRecord =
    { document : Document }


type alias Documents =
    List Document


type alias DocumentStack =
    List Document


type alias DocumentsRecord =
    { documents : Documents
    }


type SearchDomain
    = Private
    | Public
    | All


type SearchOrder
    = Viewed
    | Created
    | Updated
    | Alphabetical


type alias SearchState =
    { query : String
    , domain : SearchDomain
    , order : SearchOrder
    }


type ActiveDocumentList
    = SearchResultList
    | DocumentStackList


type DeleteState
    = Pending
    | Resting


type alias AppState =
    { activeDocumentList : ActiveDocumentList
    , online : Bool
    , deleteState : DeleteState
    , signedIn : Bool
    , authorizing : Bool
    , registerUser : Bool
    , menuDropped : Bool
    , textTypeMenuDropped : Bool
    , docTypeMenuDropped : Bool
    , textBufferDirty : Bool
    , masterDocLoaded : Bool
    , masterDocOpened : Bool
    , tickerPaused : Bool
    , page : Page
    , tool : Tool
    , editRecord : EditRecord
    , tickInterval : Float
    , seed : Int
    , command : String
    }


type alias Model =
    { window : KWindow
    , device : Device
    , counter : Int
    , appState : AppState
    , message : String
    , textInputBuffer : String
    , searchQueryInputBuffer : String
    , current_user : User
    , errorMsg : String
    , warning : String
    , current_document : Document
    , specialDocument : Document
    , master_document : Document
    , documents : Documents
    , documents2 : Documents
    , documentKey : String
    , documentDict : Dict.Dict String Document
    , documentStack : DocumentStack
    , searchState : SearchState
    , phxSocket : Phoenix.Socket.Socket Msg
    , messageInProgress : String
    , messages : List String
    , imageRecord : ImageRecord
    , fileInputId : String
    , date : Maybe Date
    , time : Maybe Time
    , fileToUpload : Maybe NativeFile
    , userList : Users
    , selectedUserName : String
    }


type alias SystemStatus =
    { online : Bool }


type alias ErrorMessage =
    String


type Msg
    = NoOp
    | AddToMasterDocument
    | AdoptChildren
    | AttachCurrentDocument String
    | AuthenticationAction
    | CancelAuthentication
    | ClearSearch
    | CompleteRegistration (Result Http.Error UserRecord)
    | CreateDocument (Result Http.Error DocumentRecord)
    | CredentialsResult (Result Http.Error CredentialsWrapper)
    | RequestDocumentDelete
    | CancelDocumentDelete
    | DeleteCurrentDocument
    | DeleteDocument (Result Http.Error ())
    | DoRender Int
    | DoSearch SearchDomain Int
    | EditSpecialDocument
    | Email String
    | FileSelected
    | FileUploaded Bool
    | Files (List NativeFile)
    | GenerateSeed
    | MigrateFromAsciidocLatex
    | GetDocuments (Result Http.Error DocumentsRecord)
    | GetHomePageForUserHomePages String String
    | GetPublicPage String
    | GetRenderedText String
    | GetUser (Result Http.Error BigUserRecord)
    | GetUserState (Result Http.Error UserStateRecord)
    | GetSpecialDocument (Result Http.Error DocumentsRecord)
    | GetMasterDocument (Result Http.Error DocumentsRecord)
    | GetTokenCompleted (Result Http.Error String)
    | GetUploadCredentials
    | GetUserDocuments (Result Http.Error DocumentsRecord)
    | GetUsers (Result Http.Error UsersRecord)
    | GoTo Page
    | GoToPage (Maybe Page)
    | GotoUserHomePages
    | GotoUserPreferencesPage
    | HandleSendError JsEncode.Value
    | ImageRead ImagePortData
    | ImageSelected
    | InitStartPage
    | InputContent String
    | InputTags String
    | LinkTo String
    | Login
    | Message String
    | Name String
    | NewDocument
    | NewSeed Int
    | Password String
    | PhoenixMsg (Phoenix.Socket.Msg Msg)
    | PutDocument (Result Http.Error ())
    | PutUser (Result Http.Error ())
    | RandomDocuments
    | RecallLastSearch
    | ReceiveChatMessage JsEncode.Value
    | ReceiveDate Date
    | ReceiveTime Time
    | ReconnectUser String
    | RecoverUserState String
    | Resize Int Int
    | Register
    | RequestDate
    | RequestTime
    | SaveCurrentDocument
    | SaveDocument (Result Http.Error ())
    | SearchForUserHomePages Int
    | SelectDocument Document
    | SelectMaster Document
    | SelectSearchMode String
    | SelectSearchOrder String
    | SelectTool Tool
    | SendMessage
    | SendToJS String
    | SetDocType String
    | SetDocumentInDict (Result Http.Error ( DocumentsRecord, String ))
    | SetMessage String
    | SetParentId String
    | SetSearchTerm String
    | SetTextType String
    | SetUserState (Result Http.Error ( DocumentsRecord, DocumentsRecord ))
    | SetupPages
    | Signout
    | SignOutOrIn
    | Tick Time
    | Title String
    | ToggleListView
    | ToggleMenu String
    | TogglePublic
    | ToggleRegister
    | ToggleUpdateRate
    | UpdateDocument
    | LatexFullRender
    | UpdateSearchQueryInputBuffer String
    | UpdateTextInputBuffer String
    | UpdateCurrentUser
    | UploadComplete (Result Http.Error String)
    | UseSearchDomain SearchDomain
    | UserHomePage
    | Username String


type Page
    = StartPage
    | LoginPage
    | PublicPage Int
    | PrivatePage Int
    | ReaderPage
    | EditorPage
    | ImagePage
    | AdminPage
    | UserHomePages
    | UserPreferencesPage


pageName : Page -> String
pageName page =
    case page of
        StartPage ->
            "Home"

        LoginPage ->
            "Sign in"

        ReaderPage ->
            "Reader"

        PublicPage _ ->
            "Reader"

        PrivatePage _ ->
            "Reader"

        EditorPage ->
            "Editor"

        ImagePage ->
            "Image"

        AdminPage ->
            "Admin"

        UserHomePages ->
            "User"

        UserPreferencesPage ->
            "User preferences"


type Tool
    = TableOfContents
    | EditorTools
    | NewDocumentTools
    | ReaderTools


type alias Flags =
    { width : Int
    , height : Int
    }
