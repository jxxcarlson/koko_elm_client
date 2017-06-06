module Views.Editor exposing (editor)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events as HE exposing (onClick)
import Css exposing (asPairs)
import Types exposing (Model, Msg)
import Test exposing (..)


import Koko.Asciidoc


styles =
    Css.asPairs >> Html.Attributes.style


editor : Model -> Html Msg
editor model =
    div []
        [ div [ id "textPane" ] [ text Test.dummyAsciidocText ]
        , div [ id "textPane2" ] [ text Test.dummyAsciidocText ]
        ]
