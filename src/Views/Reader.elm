module Views.Reader exposing (reader)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events as HE exposing (onClick)
import Css exposing (asPairs)
import Types exposing (Model, Msg)
import Views.Component exposing (toolSelectorPanel, toolSelector)


-- import Koko.Mathjax exposing (toHtml)


styles : List Css.Mixin -> Attribute msg
styles =
    Css.asPairs >> Html.Attributes.style


reader : Model -> Html Msg
reader model =
    div []
        [ div [ id "toolSelectorPanel" ] [ toolSelectorPanel model ]
          -- div [ id "toolSelectorPanel" ] [ toolSelectorPanel model ]
        , div [ id "toolPane" ] [ toolSelector model ]
        , div [ id "titlePane" ] [ text model.current_document.title ]
          -- HERE use the node with id = rendered_text2 in JS-land.
        ]
