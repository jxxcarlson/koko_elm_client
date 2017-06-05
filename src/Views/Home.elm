module Views.Home exposing (home)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events as HE exposing (onClick)
import Css exposing (asPairs)
import Types exposing (Model, Msg)
import Views.Signin exposing (..)


styles =
    Css.asPairs >> Html.Attributes.style


home : Model -> Html Msg
home model =
    div [] [ signin model ]
