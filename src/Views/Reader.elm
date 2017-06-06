module Views.Reader exposing (reader)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events as HE exposing (onClick)
import Css exposing (asPairs)
import Types exposing (Model, Msg)
import Test exposing (..)
import Koko.Mathjax exposing (toHtml)


styles =
    Css.asPairs >> Html.Attributes.style


reader : Model -> Html Msg
reader model =
    div []
        [ div [ id "textPane" ] [ toHtml [] model.current_document.rendered_content ]
        ]
