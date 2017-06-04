module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Window exposing (..)
import Css exposing (asPairs)
import Test exposing (dummyText)


type alias KWindow =
    { width : Int
    , height : Int
    }


type alias Model =
    { window : KWindow
    , message : String
    }


type Msg
    = NoOp
    | Resize Int Int


updateWindow : Model -> Int -> Int -> Model
updateWindow model w h =
    let
        new_window =
            KWindow w h
    in
        { model | window = new_window, message = "w: " ++ (toString model.window.width) ++ ", h: " ++ (toString model.window.height) }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Resize w h ->
            ( (updateWindow model w h), Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Window.resizes (\{ width, height } -> Resize width height)


windowCss model =
    [ Css.width (Css.px ((toFloat model.window.width) - 100.0))
    , Css.height (Css.px (0.9 * (toFloat model.window.height - 575.0)))
    ]


view : Model -> Html Msg
view model =
    --
    div []
        [ div [ id "header" ] [ text "Noteshare" ]
        , div [ id "textPane" ] [ text Test.dummyText ]
        , div [ id "footer" ] [ text model.message ]
        ]


type alias Flags =
    { width : Int
    , height : Int
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( Model (KWindow flags.width flags.height) "Start!"
    , Cmd.none
    )


main =
    programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
