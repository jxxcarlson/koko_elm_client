module Types exposing (..)

import Http
import Time exposing (Time)
import Phoenix.Socket
import Json.Encode as JsEncode
import Date exposing(Date)
import Dict
import Image.FileReader as FileReader exposing (NativeFile)


type alias User =
    { name : String
    , id: Int
    , username : String
    , email : String
    , blurb : String
    , password : String
    , token : String
    , admin : Bool }

type alias Users = List User
{-|
  Use to transfer data to JS-world. Does not contani password
-}

type alias UsersRecord =
    { users : List User
    }


type alias LoginUserRecord =
    { name : String, username : String, id: String, email : String, token : String }


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
defaultImage = Image "data:image/gif;base64,R0lGODlhAQABAIAAAP///wAAACH5BAEAAAAALAAAAAABAAEAAAICRAEAOw==" "fileName"

type alias ImageRecord =
    { id : String
    , mImage : Maybe Image
  }

defaultImageRecord : ImageRecord
defaultImageRecord = ImageRecord "ImageInputId" Nothing

type alias Credentials = {
  signature: String,
  policy: String,
  key: String,
  acl: String,
  awsAccessKeyId: String,
  date: String
}

type alias CredentialsWrapper = {
  credentials: Credentials,
  url: String
}

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
    , parent_title: String
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


type alias AppState =
    { online : Bool
    , signedIn : Bool
    , authorizing : Bool
    , registerUser : Bool
    , menuDropped : Bool
    , textTypeMenuDropped : Bool
    , docTypeMenuDropped : Bool
    , textBufferDirty : Bool
    , masterDocLoaded : Bool
    , tickerPaused : Bool
    , page : Page
    , tool : Tool
    , textBuffer : String
    , tickInterval : Float
    , command : String
    }


type alias Model =
    { window : KWindow
    , counter : Int
    , appState : AppState
    , message : String
    , textInputBuffer: String
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

type alias ErrorMessage = String

type Msg
    = NoOp
    | Resize Int Int
    | GoTo Page
    | Login
    | ReconnectUser String
    | Register
    | CompleteRegistration (Result Http.Error LoginUserRecord)
    | Signout
    | AuthenticationAction
    | CancelAuthentication
    | ToggleRegister
    | ToggleMenu String
    | TogglePublic
    | ToggleUpdateRate
    | GetTokenCompleted (Result Http.Error String)
    | GetDocuments (Result Http.Error String)
    | GetUserDocuments (Result Http.Error DocumentsRecord)
    | GetSpecialDocument (Result Http.Error DocumentsRecord)
    | PutDocument (Result Http.Error ())
    | CreateDocument (Result Http.Error DocumentRecord)
    | DeleteDocument (Result Http.Error ())
    | NewDocument
    | DeleteCurrentDocument
    | GetUsers (Result Http.Error UsersRecord)
    | SearchForUserHomePages Int
    | Title String
    | SetTextType String
    | SetDocType String
    | AdoptChildren
    | InputTags String
    | InputContent String
    | UpdateTextInputBuffer String
    | SetParentId String
    | AddToMasterDocument
    | AttachCurrentDocument String
    | SaveCurrentDocument
    | Email String
    | Password String
    | Name String
    | Username String
    | SelectTool Tool
    | SetSearchTerm String
    | SelectSearchMode String
    | SelectSearchOrder String
    | DoSearch SearchDomain Int
    | ClearSearch
    | RecallLastSearch
    | GetPublicPage String
    | DoRender Int
    | GetRenderedText String
    | SelectDocument Document
    | SelectMaster Document
    | UseSearchDomain SearchDomain
    | Tick Time
    | Refresh
    | SendToJS String
    | SetupPages
      -- Phoenix channels
    | PhoenixMsg (Phoenix.Socket.Msg Msg)
    | SetMessage String
    | SendMessage
    | ReceiveChatMessage JsEncode.Value
    | HandleSendError JsEncode.Value
    | ImageSelected
    | ImageRead ImagePortData
    | GetUploadCredentials
    | CredentialsResult (Result Http.Error CredentialsWrapper)
    | Files (List NativeFile)
    | UploadComplete (Result Http.Error String)
    | FileSelected
    | FileUploaded Bool
    | UserHomePage
    | GotoUserHomePages
    | GetHomePageForUserHomePages String String
    | InitHomePage
    | GoToPage (Maybe Page)
    | LinkTo String
    | RequestDate
    | RequestTime
    | ReceiveDate Date
    | ReceiveTime Time


type Page
    = HomePage
    | PublicPage Int
    | PrivatePage Int
    | ReaderPage
    | EditorPage
    | ImagePage
    | AdminPage
    | UserHomePages


pageName : Page -> String
pageName page =
    case page of
        HomePage ->
            "Home"

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

type Tool
    = TableOfContents
    | EditorTools
    | NewDocumentTools
    | ReaderTools


type alias Flags =
    { width : Int
    , height : Int
    }

defaultAttributes : DocumentAttributes
defaultAttributes =
    DocumentAttributes False "adoc" "standard" 0

startDocument : Document
startDocument = {
  id = 0
  , identifier = "nullDocument"
  , author_id = 0
  , author_name = ""
  , title = "Welcome"
  , content = "Welcome to noteshare"
  , rendered_content = "Welcome to noteshare"
  , attributes = defaultAttributes
  , tags = []
  , children = []
  , parent_id = 0
  , parent_title = "String"
  }


blankDocument : Document
blankDocument = {
  id = 0
  , identifier = "blank"
  , author_id = 0
  , author_name = ""
  , title = "New Document"
  , content = "Write content here"
  , rendered_content = "Write content here"
  , attributes = defaultAttributes
  , tags = []
  , children = []
  , parent_id = 0
  , parent_title = "String"
  }

emptyDocument : Document
emptyDocument = {
  id = 0
  , identifier = "empty"
  , author_id = 0
  , author_name = ""
  , title = ""
  , content = ""
  , rendered_content = ""
  , attributes = defaultAttributes
  , tags = []
  , children = []
  , parent_id = 0
  , parent_title = "String"
  }

defaultDocument : Document
defaultDocument =  {
  id = 0
  , identifier = "nullDocument"
  , author_id = 0
  , author_name = ""
  , title = "Oops!"
  , content = "Page not found or access restricted"
  , rendered_content = "Page not found or access restricted"
  , attributes = defaultAttributes
  , tags = []
  , children = []
  , parent_id = 0
  , parent_title = "String"
  }

defaultMasterDocument : Document
defaultMasterDocument =  {
  id = 0
  , identifier = "nullMasterDocument"
  , author_id = 0
  , author_name = ""
  , title = "Null master document"
  , content = "nothing"
  , rendered_content = "nothing"
  , attributes = defaultAttributes
  , tags = []
  , children = []
  , parent_id = 0
  , parent_title = "String"
  }
