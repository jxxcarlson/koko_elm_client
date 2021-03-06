module Types exposing (..)

import Date exposing (Date)
import Dict
import Http
import Image.FileReader as FileReader exposing (NativeFile)
import Json.Encode
import MiniLatex.Differ exposing (EditRecord)
import Phoenix.Socket
import Time exposing (Time)


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
    , date : String
    , credential : String
    , algorithm : String
    , policy : String
    , key : String
    , acl : String
    }


type alias CredentialsWrapper =
    { credentials : Credentials
    , url : String
    }


type alias DocumentDict =
    Dict.Dict String Document


type alias AccessDict =
    Dict.Dict String String


type alias DocumentAttributes =
    { public : Bool
    , textType : String
    , docType : String
    , level : Int
    , archive : String
    , version : Int
    , lastViewed : Maybe Time
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
    , access : AccessDict
    , title : String
    , content : String
    , rendered_content : String
    , attributes : DocumentAttributes
    , tags : List String
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
    | Shared
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
    , textNeedsUpdate : Bool
    , masterDocLoaded : Bool
    , masterDocOpened : Bool
    , tickerPaused : Bool
    , page : Page
    , tool : Tool
    , editRecord : EditRecord
    , tickInterval : Float
    , seed : Int
    , command : String
    , shareDocumentCommand : String
    }


type alias Model =
    { window : KWindow
    , device : Device
    , counter : Int
    , appState : AppState
    , message : String
    , textInputBuffer : String
    , textToExport : String
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
    , tick : Int
    , lastEditTime : Maybe Time
    , fileToUpload : Maybe NativeFile
    , userList : Users
    , selectedUserName : String
    , userStateRecord : UserStateRecord
    }


type alias ErrorMessage =
    String



-- https://www.reddit.com/r/elm/comments/5jd2xn/how_to_structure_elm_with_multiple_models/dbuu0m4/


type DocMsg
    = AddToMasterDocument
    | InputShareDocumentCommand String
    | UpdateShareData
    | IncrementVersion
    | SetRepositoryName String
    | AdoptChildren
    | AttachCurrentDocument String
    | CreateDocument (Result Http.Error DocumentRecord)
    | RequestDocumentDelete
    | CancelDocumentDelete
    | CompileMaster
    | DeleteCurrentDocument
    | DeleteDocument (Result Http.Error ())
    | DoRender Int
    | EditSpecialDocument
    | GetDiary
    | GetDocuments (Result Http.Error DocumentsRecord)
    | GetUserDocuments (Result Http.Error DocumentsRecord)
    | GetSpecialDocument (Result Http.Error DocumentsRecord)
    | GetMasterDocument (Result Http.Error DocumentsRecord)
    | InputContent String
    | InputTags String
    | InputTextForExport String
    | LatexFullRender
    | LoadContent (Result Http.Error DocumentsRecord)
    | LoadContentAndRender (Result Http.Error DocumentsRecord)
    | NewDocument
    | NewDiaryEntry
    | PutDocument (Result Http.Error ())
    | RandomDocuments
    | RenumberDocuments
    | SaveCurrentDocument
    | SaveDocument (Result Http.Error ())
    | SelectDocument Document
    | SelectMaster Document
    | SetDocType String
    | SetDocumentInDict (Result Http.Error ( DocumentsRecord, String ))
    | SetParentId String
    | SetTextType String
    | Title String
    | TogglePublic
    | UpdateDocument
    | UpdateTextInputBuffer String
    | MigrateFromAsciidocLatex
    | GenerateSeed
    | NewSeed Int


type AuthMsg
    = AuthenticationAction
    | CancelAuthentication
    | Email String
    | CompleteRegistration (Result Http.Error UserRecord)
    | GetTokenCompleted (Result Http.Error String)
    | Login
    | Password String
    | Name String
    | Register
    | Signout
    | SignOutOrIn
    | ToggleRegister
    | Username String


type ChannelMsg
    = SetMessage String
    | SendMessage
    | ReceiveChatMessage Json.Encode.Value
    | HandleSendError Json.Encode.Value
    | PhoenixMsg (Phoenix.Socket.Msg Msg)


type ImageMsg
    = ImageRead ImagePortData
    | ImageSelected
    | GetUploadCredentials
    | CredentialsResult (Result Http.Error CredentialsWrapper)
    | UploadComplete (Result Http.Error String)
    | FileSelected
    | FileUploaded Bool
    | Files (List NativeFile)


type SearchMsg
    = ClearSearch
    | DoSearch SearchDomain Int
    | RecallLastSearch
    | SelectSearchMode String
    | SelectSearchOrder String
    | SearchForUserHomePages Int
    | SetSearchTerm String
    | UpdateSearchQueryInputBuffer String
    | UseSearchDomain SearchDomain


type PageMsg
    = GetPublicPage String
    | GetHomePageForUserHomePages String String
    | GoTo Page
    | GoToPage (Maybe Page)
    | GotoUserHomePages
    | GotoUserPreferencesPage
    | InitStartPage
    | SetupPages
    | UserHomePage


type PeriodicMsg
    = Tick Time
    | ReceiveDate Date
    | ReceiveTime Time
    | RequestDate
    | RequestTime


type UIMsg
    = ToggleListView
    | ToggleUpdateRate
    | ToggleMenu String
    | Message String
    | SelectTool Tool


type UserMsg
    = UpdateCurrentUser
    | GetUser (Result Http.Error BigUserRecord)
    | GetUsers (Result Http.Error UsersRecord)
    | GetUserState (Result Http.Error UserStateRecord)
    | PutUser (Result Http.Error ())
    | SetUserState (Result Http.Error ( DocumentsRecord, DocumentsRecord ))


type WindowMsg
    = Resize Int Int


type InfoForOutside
    = PutTextToRender Json.Encode.Value
    | WindowData Json.Encode.Value
    | UserData Json.Encode.Value
    | UserState Json.Encode.Value
    | SaveDocumentStack Json.Encode.Value
    | AskToReconnectUser Json.Encode.Value
    | AskToRecoverUserState Json.Encode.Value
    | DisconnectUser Json.Encode.Value


type InfoForElm
    = RenderedText String
    | RecoveredUserState UserStateRecord
    | UserLoginInfo LoginLocalStorageRecord


type alias GenericOutsideData =
    { tag : String, data : Json.Encode.Value }


type Msg
    = NoOp
    | LogErr String
    | AuthMsg AuthMsg
    | ChannelMsg ChannelMsg
    | UIMsg UIMsg
    | DocMsg DocMsg
    | ImageMsg ImageMsg
    | PageMsg PageMsg
    | PeriodicMsg PeriodicMsg
    | SearchMsg SearchMsg
    | UserMsg UserMsg
    | WindowMsg WindowMsg
    | Outside InfoForElm


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


type alias Flags =
    { width : Int
    , height : Int
    }
