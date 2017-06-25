module Views.Search exposing (documentListView)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events as HE exposing (on, onClick, onInput)
import Utility exposing (onKeyUp)


-- import Types exposing(Model, Msg)

import Types exposing (..)


documentListView : Model -> Html Msg
documentListView model =
    div [ id "documentListView" ]
        [ ul [] (List.map (viewTitle model.current_document) model.documents)
        ]


viewTitle : Document -> Document -> Html Msg
viewTitle selectedDocument document =
    li
        [ classList [ ( "selected", selectedDocument.title == document.title ) ]
        , onClick (SelectDocument document)
        ]
        [ text document.title ]
