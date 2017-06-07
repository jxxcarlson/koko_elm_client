module Views.Component exposing (..)

import Html exposing (..)
import Html.Attributes as HA exposing (..)
import Html.Events as HE exposing (onClick)
import Utility exposing (styles)
import Types exposing (..)
import Css exposing (..)
import Views.Search exposing (documentListView)


selectedClass : Page -> Model -> String
selectedClass page model =
    if page == model.page then
        "isSelected"
    else
        "isNotSelected"


selectedToolClass : Tool -> Model -> String
selectedToolClass tool model =
    if tool == model.tool then
        "isSelected"
    else
        "isNotSelected"


selectedToolClass2 : Tool -> Model -> String
selectedToolClass2 tool model =
    if tool == model.tool then
        "isSelected2"
    else
        "isNotSelected2"


readerToolSelectorPanel : Model -> Html Msg
readerToolSelectorPanel model =
    span
        [ styles [ Css.marginLeft (Css.px 8.0) ] ]
        [ button [ onClick (SelectTool TableOfContents), HA.class "smallButton", HA.class (selectedToolClass2 TableOfContents model) ]
            [ Html.text "TOC" ]
        , button
            [ onClick (SelectTool EditorTools), HA.class "smallButton", HA.class (selectedToolClass2 ReaderTools model) ]
            [ Html.text "Tools" ]
        ]


editorToolSelectorPanel : Model -> Html Msg
editorToolSelectorPanel model =
    span
        [ styles [ Css.marginLeft (Css.px 8.0) ] ]
        [ button [ onClick (SelectTool TableOfContents), HA.class "smallButton", HA.class (selectedToolClass2 TableOfContents model) ]
            [ Html.text "TOC" ]
        , button
            [ onClick (SelectTool EditorTools), HA.class "smallButton", HA.class (selectedToolClass2 EditorTools model) ]
            [ Html.text "Tools" ]
        ]


readerToolSelector : Model -> Html Msg
readerToolSelector model =
    case model.tool of
        TableOfContents ->
            tableOfContents model

        ReaderTools ->
            readerTools model


toolSelector : Model -> Page -> Html Msg
toolSelector model page =
    case page of
        EditorPage ->
            case model.editor_tool of
                TableOfContents ->
                    tableOfContents model

                EditorTools ->
                    editorTools model


editorTools : Model -> Html Msg
editorTools model =
    div [] [ Html.text "Tools" ]


readerTools : Model -> Html Msg
readerTools model =
    div [] [ Html.text "Reader Tools" ]


tableOfContents : Model -> Html Msg
tableOfContents model =
    div [] [ documentListView model ]


pageSelector : Model -> Html Msg
pageSelector model =
    span [ styles [ Css.marginLeft (Css.px 100.0) ] ]
        [ button [ onClick (GoTo HomePage), HA.class (selectedClass HomePage model) ]
            [ Html.text "Home" ]
        , button
            [ onClick (GoTo ReaderPage), HA.class (selectedClass ReaderPage model) ]
            [ Html.text "Reader" ]
        , button
            [ onClick (GoTo EditorPage), HA.class (selectedClass EditorPage model) ]
            [ Html.text "Editor" ]
        ]
