module Types exposing (..)

import Http
import Time exposing (Time)
import Phoenix.Socket
import Json.Encode as JsEncode
import Date exposing(Date)




type alias User =
    { name : String, username : String, email : String, password : String, token : String }


{-|
  Use to transfer data to JS-world. Does not contani password
-}
type alias UserRecord =
    { name : String, username : String, email : String, token : String }


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


type alias SearchState =
    { query : String
    , domain : SearchDomain
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
    , page : Page
    , tool : Tool
    , textBuffer : String
    , command : String
    }


type alias Model =
    { window : KWindow
    , counter : Int
    , appState : AppState
    , message : String
    , current_user : User
    , errorMsg : String
    , info : String
    , current_document : Document
    , master_document : Document
    , documents : Documents
    , documents2 : Documents
    , documentStack : DocumentStack
    , searchState : SearchState
    , phxSocket : Phoenix.Socket.Socket Msg
    , messageInProgress : String
    , messages : List String
    , imageRecord : ImageRecord
    , fileInputId : String
    , date : Maybe Date
    }


type alias SystemStatus =
    { online : Bool }


type Msg
    = NoOp
    | Resize Int Int
    | GoTo Page
    | Login
    | ReconnectUser String
    | Register
    | CompleteRegistration (Result Http.Error UserRecord)
    | Signout
    | AuthenticationAction
    | CancelAuthentication
    | ToggleRegister
    | ToggleMenu String
    | TogglePublic
    | GetTokenCompleted (Result Http.Error String)
    | GetDocuments (Result Http.Error String)
    | GetUserDocuments (Result Http.Error DocumentsRecord)
    | PutDocument (Result Http.Error ())
    | CreateDocument (Result Http.Error DocumentRecord)
    | DeleteDocument (Result Http.Error ())
    | NewDocument
    | DeleteCurrentDocument
    | Title String
    | SetTextType String
    | SetDocType String
    | AdoptChildren
    | InputTags String
    | InputContent String
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
    | DoSearch SearchDomain Int
    | ClearSearch
    | RecallLastSearch
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
    | UploadComplete (Result Http.Error String)
    | FileSelected
    | FileUploaded Bool
    | UserHomePage
    | InitHomePage
    | GoToPage (Maybe Page)
    | LinkTo String
    | RequestDate
    | ReceiveDate Date


type Page
    = HomePage
    | PublicPage Int
    | PrivatePage Int
    | ReaderPage
    | EditorPage
    | ImagePage


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

defaultDocument : Document
defaultDocument =
    Document 0 "nullDocument" 0 "Default document" "nothing" "nothing" defaultAttributes [] [] 0 ""

defaultMasterDocument : Document
defaultMasterDocument =
    Document 0 "nullMasterDocument" 0 "Null master document" "nothing" "nothing" defaultAttributes [] [] 0 ""
