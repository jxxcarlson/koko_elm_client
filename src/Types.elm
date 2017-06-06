module Types exposing (..)

import Http
import Navigation


type alias User =
    { name : String, username : String, email : String, password : String, token : String }


type alias KWindow =
    { width : Int
    , height : Int
    }


type alias Document =
    { title : String
    , content : String
    , rendered_content : String
    }


type alias Model =
    { window : KWindow
    , page : Page
    , message : String
    , current_user : User
    , registerUser : Bool
    , errorMsg : String
    , info : String
    , current_document : Document
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
    | Email String
    | Password String
    | Name String
    | Username String
    | SendToJs String
    | UpdateStr String


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


type alias Flags =
    { width : Int
    , height : Int
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        current_user =
            User "" "" "" "" ""

        registerUser =
            False

        text =
            "The formula is $\\int_0^1 x^n = \\frac{1}{n}$"

        doc =
            Document "Test document" text text
    in
        ( Model
            (KWindow flags.width flags.height)
            HomePage
            "Please sign in"
            current_user
            registerUser
            ""
            ""
            doc
        , Cmd.none
        )
