module Types exposing (..)

import Http
import Navigation


type alias User =
    { name : String, username : String, email : String, password : String, token : String }


type alias KWindow =
    { width : Int
    , height : Int
    }


type alias Model =
    { window : KWindow
    , page : Page
    , message : String
    , current_user : User
    , registerUser : Bool
    , errorMsg : String
    , info : String
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
