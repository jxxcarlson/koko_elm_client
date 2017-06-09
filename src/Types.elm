module Types exposing (..)

import Http
import Time exposing (Time)


type alias User =
    { name : String, username : String, email : String, password : String, token : String }


type alias KWindow =
    { width : Int
    , height : Int
    }


type alias Document =
    { id : Int
    , author_id : Int
    , title : String
    , content : String
    , rendered_content : String
    }


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


type alias Model =
    { window : KWindow
    , page : Page
    , tool : Tool
    , message : String
    , current_user : User
    , registerUser : Bool
    , errorMsg : String
    , info : String
    , current_document : Document
    , documents : Documents
    , searchState : SearchState
    }


type Msg
    = NoOp
    | Resize Int Int
    | GoTo Page
    | Login
    | Register
    | Signout
    | ToggleRegister
    | GetTokenCompleted (Result Http.Error String)
    | GetDocuments (Result Http.Error String)
    | InputContent String
    | Email String
    | Password String
    | Name String
    | Username String
    | SelectTool Tool
    | SetSearchTerm String
    | KeyUp Int
    | SelectDocument Document
    | UseSearchDomain SearchDomain
    | Tick Time
    | Refresh


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
    | ReaderTools


type alias Flags =
    { width : Int
    , height : Int
    }


defaultDocument =
    Document 0 0 "Default document" "Yada" "Yada"


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        current_user =
            User "" "" "" "" ""

        registerUser =
            False

        title =
            "Test document"

        text =
            "The correct formula is $$\\int_0^1 x^n = \\frac{1}{n}$$"

        rendered_text =
            "The *RENDERED formula* is $$\\int_0^1 x^n = \\frac{1}{n}$$ (HA HA HA!)"

        doc =
            Document 0 0 title text rendered_text

        searchState =
            SearchState "" Public
    in
        ( Model
            (KWindow flags.width flags.height)
            HomePage
            TableOfContents
            "Please sign in"
            current_user
            registerUser
            ""
            ""
            doc
            [ doc ]
            searchState
        , Cmd.none
        )
