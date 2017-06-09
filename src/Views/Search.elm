module Views.Search exposing (documentSearchForm, documentListView)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events as HE exposing (on, onClick, onInput)
import Utility exposing (onKeyUp)


-- import Types exposing(Model, Msg)

import Types exposing (..)


documentSearchForm : Model -> Html Msg
documentSearchForm model =
    div [ id "SearchForm" ]
        [ input
            [ id "searchInputField"
            , type_ "text"
            , placeholder "Search"
              --, Html.Attributes.value model.input_text
            , onInput SetSearchTerm
            , Utility.onKeyUp KeyUp
            ]
            []
        ]


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
