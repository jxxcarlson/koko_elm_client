module Views.Reader exposing (menubar)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events as HE exposing (onClick)
import Css exposing (asPairs)


styles =
    Css.asPairs >> Html.Attributes.style


menubar : Model -> Html Msg
menubar model =
    [ text "Reader" ]
