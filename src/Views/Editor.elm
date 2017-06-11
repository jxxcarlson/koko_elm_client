module Views.Editor exposing (editor)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events as HE exposing (..)


--onClick, onInput, on

import Views.Component exposing (toolSelectorPanel, toolSelector)
import Action.Document exposing (wordCount)
import Utility exposing (onKeyUp)
import Html.Keyed as Keyed


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
            , Utility.onKeyUp DoRender
            ]
            []
        , div [ id "editor_info_pane" ] [ text ("Words: " ++ (toString <| wordCount <| model.current_document)) ]
          -- HERE use the node with id = rendered_text2 in JS-land.
        ]
