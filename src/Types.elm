module Types exposing (..)

import Http
import Time exposing (Time)
import Phoenix.Socket
import Json.Encode as JsEncode




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

-- defaultImage :: Image
defaultImage = Image "data:image/gif;base64,R0lGODlhAQABAIAAAP///wAAACH5BAEAAAAALAAAAAABAAEAAAICRAEAOw==" "fileName"

type alias ImageRecord =
    { id : String
    , mImage : Maybe Image
  }

-- defaultImageRecord :: ImageRecord
defaultImageRecord = ImageRecord "ImageInputId" Nothing


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
    , documents : Documents
    , documents2 : Documents
    , searchState : SearchState
    , phxSocket : Phoenix.Socket.Socket Msg
    , messageInProgress : String
    , messages : List String
    , imageRecord : ImageRecord
    , fileInputId : String
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
    | InputTags String
    | InputContent String
    | SetParentId String
    | SaveCurrentDocument
    | Email String
    | Password String
    | Name String
    | Username String
    | SelectTool Tool
    | SetSearchTerm String
    | DoSearch SearchDomain Int
    | RecallLastSearch
    | DoRender Int
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
    | FileSelected
    | FileUploaded Bool


type Page
    = HomePage
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

        EditorPage ->
            "Editor"

        ImagePage ->
          "Image"


type Tool
    = TableOfContents
    | EditorTools
    | DocumentParameterTools
    | ReaderTools


type alias Flags =
    { width : Int
    , height : Int
    }


defaultAttributes =
    DocumentAttributes False "adoc" "standard" 0


defaultDocument =
    Document 0 "abcd" 0 "Default document" "Yada" "Yada" defaultAttributes [] [] 0 ""
