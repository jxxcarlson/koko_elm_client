module Views.Home exposing (home)

import Html exposing (..)
import Html.Attributes exposing (..)
import Css exposing (asPairs)
import Types exposing (Model, Msg)
import Views.Signin exposing (..)
import Utility exposing (youtube)


styles =
    Css.asPairs >> Html.Attributes.style


home : Model -> Html Msg
home model =
    div []
        [ signin model
        , Utility.youtube "https://www.youtube.com/embed/EsTgr-n53Ow"
        ]
