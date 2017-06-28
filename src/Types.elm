module Types exposing (..)

import HttpBuilder
import Http
import Time exposing (Time)


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


type alias DocumentAttributes =
    { public : Bool
    , textType : String
    , docType : String
    }


type alias DocumentAttributesRecord =
    { attributes : DocumentAttributes }


type alias Document =
    { id : Int
    , author_id : Int
    , title : String
    , content : String
    , rendered_content : String
    , attributes : DocumentAttributes
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
    , textBufferDirty : Bool
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
    , searchState : SearchState
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
    | ToggleRegister
    | ToggleMenu
    | TogglePublic
    | GetTokenCompleted (Result Http.Error String)
    | GetDocuments (Result Http.Error String)
    | GetUserDocuments (Result Http.Error DocumentsRecord)
    | PutDocument (Result Http.Error ())
    | CreateDocument (Result Http.Error DocumentRecord)
    | NewDocument
    | Title String
    | InputContent String
    | Email String
    | Password String
    | Name String
    | Username String
    | SelectTool Tool
    | SetSearchTerm String
    | DoSearch SearchDomain Int
    | DoRender Int
    | SelectDocument Document
    | UseSearchDomain SearchDomain
    | Tick Time
    | Refresh
    | SendToJS String
    | SetupPages


type Page
    = HomePage
    | ReaderPage
    | EditorPage


pageName : Page -> String
pageName page =
    case page of
        HomePage ->
            "Home"

        ReaderPage ->
            "Reader"

        EditorPage ->
            "Editor"


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
    DocumentAttributes False "adoc" "standard"


defaultDocument =
    Document 0 0 "Default document" "Yada" "Yada" defaultAttributes
