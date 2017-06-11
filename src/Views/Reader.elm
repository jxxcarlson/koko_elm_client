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
          -- , div [ id "textPane" ] [ text model.current_document.rendered_content ]
          -- , div [ id "textPane" ] [ toHtml [] model.current_document.rendered_content ]
        ]
