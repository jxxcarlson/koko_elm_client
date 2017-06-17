module Views.Editor exposing (editor)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events as HE exposing (..)
import Html.Keyed as Keyed


--onClick, onInput, on

import Views.Component exposing (toolSelectorPanel, toolSelector)
import Action.Document exposing (wordCount)
import Utility exposing (onKeyUp)


-- import Css exposing (asPairs)

import Types exposing (..)


-- import Test exposing (..)


editor : Model -> Html Msg
editor model =
    div
        []
        [ div [ id "toolSelectorPanel" ] [ toolSelectorPanel model ]
        , div [ id "toolPane" ] [ toolSelector model ]
        , input [ id "titlePane", type_ "text", placeholder "title", onInput Title, value model.current_document.title ] []
          -- , div [ id "titlePane" ] [ text model.current_document.title ]
          --, pre [ id "editPane" ] [ text model.current_document.content ]
        , Keyed.node "textarea"
            [ id "editPane"
            , defaultValue model.current_document.content
            , HE.onInput InputContent
            , Utility.onKeyUp DoRender
            ]
            []
        , div [ id "editor_info_pane" ] [ text ("Words: " ++ (toString <| wordCount <| model.current_document)) ]
          -- HERE use the node with id = rendered_text2 in JS-land.
        ]



-- redirect location = Cmd.batch [formReset (), Navigation.newUrl <| "#/" ++ location]
-- document.forms[0].reset()
-- https://github.com/etaque/elm-form/issues/54
-- https://github.com/evancz/elm-html/pull/81
-- https://groups.google.com/forum/#!msg/elm-discuss/1QYrEKC2Y2o/PTujCN0NEAAJ
-- ISSUE ABOVE: value versus defaultValue


newDocumentForm model =
    div [ id "newDocumentForm" ]
        [ input [ id "title", type_ "text", placeholder "title", onInput Title ] []
        , br [] []
        , br [] []
        , button [ id "newDocumentBttton", onClick NewDocument ] [ text "Create" ]
        , br [] []
        , br [] []
        , p [] [ text model.info ]
        ]
