module Views.Search exposing (documentSearchForm, documentListView)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events as HE exposing (on, onClick, onInput)
import Json.Decode as Json exposing (int, list, string, float, Decoder)


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
            , onKeyUp KeyUp
            ]
            []
        ]


onKeyUp : (Int -> msg) -> Attribute msg
onKeyUp tagger =
    on "keyup" (Json.map tagger HE.keyCode)


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
