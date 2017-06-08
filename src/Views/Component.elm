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


searchOptionControl : Model -> Html Msg
searchOptionControl model =
    div []
        [ fieldset [ HA.id "searchOptionControl" ]
            [ label []
                [ input [ type_ "radio", name "searchDomain", onClick (UseSearchDomain Private) ] []
                , Html.text "Private"
                ]
            , label []
                [ input [ type_ "radio", name "searchDomain", onClick (UseSearchDomain Public) ] []
                , Html.text "Public"
                ]
            ]
        ]


readerToolSelectorPanel : Model -> Html Msg
readerToolSelectorPanel model =
    span
        [ styles [ Css.marginLeft (Css.px 8.0) ] ]
        [ button [ onClick (SelectTool TableOfContents), HA.class "smallButton", HA.class (selectedToolClass TableOfContents model) ]
            [ Html.text "TOC" ]
        , button
            [ onClick (SelectTool EditorTools), HA.class "smallButton", HA.class (selectedToolClass ReaderTools model) ]
            [ Html.text "Tools" ]
        ]


toolSelectorPanel : Model -> Html Msg
toolSelectorPanel model =
    span
        [ styles [ Css.marginLeft (Css.px 8.0) ] ]
        [ button [ onClick (SelectTool TableOfContents), HA.class "smallButton", HA.class (selectedToolClass TableOfContents model) ]
            [ Html.text "TOC" ]
        , button
            [ onClick (SelectTool EditorTools), HA.class "smallButton", HA.class (selectedToolClass EditorTools model) ]
            [ Html.text "Tools" ]
        ]


toolSelector : Model -> Html Msg
toolSelector model =
    case model.tool of
        TableOfContents ->
            tableOfContents model

        EditorTools ->
            editorTools model

        ReaderTools ->
            readerTools model


editorTools : Model -> Html Msg
editorTools model =
    div [] [ searchOptionControl model ]


readerTools : Model -> Html Msg
readerTools model =
    div [] [ searchOptionControl model ]


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
