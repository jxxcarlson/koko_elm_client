module Views.Editor exposing (editor)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events as HE exposing (..)


--onClick, onInput, on

import Views.Component exposing (toolSelectorPanel, toolSelector)
import Koko.Mathjax exposing (toHtml)
import Action.Document exposing (wordCount)
import Utility exposing (onKeyUp)


-- import Css exposing (asPairs)

import Types exposing (..)


-- import Test exposing (..)


editor : Model -> Html Msg
editor model =
    div []
        [ div [ id "toolSelectorPanel" ] [ toolSelectorPanel model ]
        , div [ id "toolPane" ] [ toolSelector model ]
        , div [ id "titlePane" ] [ text model.current_document.title ]
          --, pre [ id "editPane" ] [ text model.current_document.content ]
        , textarea
            [ id "editPane"
            , value model.current_document.content
            , HE.onInput InputContent
            , Utility.onKeyUp KeyUp
            ]
            []
        , div [ id "editor_info_pane" ] [ text ("Words: " ++ (toString <| wordCount <| model.current_document)) ]
        , div [ id "editPane2" ] [ text model.current_document.rendered_content ]
          --, div [ id "editPane2" ] [ toHtml [] model.current_document.rendered_content ]
        ]
