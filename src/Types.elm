module Types exposing (..)

import Http
import Navigation


type alias KWindow =
    { width : Int
    , height : Int
    }


type alias Model =
    { window : KWindow
    , page : Page
    , message : String
    }


type Msg
    = NoOp
    | Resize Int Int
    | GoTo Page


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
