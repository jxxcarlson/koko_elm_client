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
    if page == model.appState.page then
        "isSelected"
    else
        "isNotSelected"


selectedToolClass : Tool -> Model -> String
selectedToolClass tool model =
    if tool == model.appState.tool then
        "isSelected"
    else
        "isNotSelected"


searchOptionControl : Model -> Html Msg
searchOptionControl model =
    div []
        [ strong []
            [ Html.text "Search type" ]
        , br
            []
            []
        , label [ HA.class "radio_label" ]
            [ input
                [ type_ "radio"
                , name "searchDomain"
                , onClick (UseSearchDomain Private)
                , HA.class "my_radio_button"
                , HA.disabled (privateSearchDisabled model)
                , HA.checked (searchDomainChecked model Private)
                ]
                []
            , Html.text "My Documents"
            ]
        , br [] []
        , label [ HA.class "radio_label" ]
            [ input
                [ type_ "radio"
                , name "searchDomain"
                , onClick (UseSearchDomain Public)
                , HA.class "my_radio_button"
                , HA.checked (searchDomainChecked model Public)
                ]
                []
            , Html.text "Public Documents"
            ]
        ]


searchDomainChecked : Model -> SearchDomain -> Bool
searchDomainChecked model domain =
    model.searchState.domain == domain


privateSearchDisabled : Model -> Bool
privateSearchDisabled model =
    if model.current_user.token == "" then
        True
    else
        False


toolSelectorPanel : Model -> Html Msg
toolSelectorPanel model =
    span
        [ styles [ Css.marginLeft (Css.px 8.0) ] ]
        [ button [ onClick (SelectTool TableOfContents), HA.class "smallButton", HA.class (selectedToolClass TableOfContents model) ]
            [ Html.text "TOC" ]
        , button
            [ onClick (SelectTool EditorTools), HA.class "smallButton", HA.class (selectedToolClass EditorTools model) ]
            [ Html.text "Tools" ]
        , button
            [ onClick Refresh, HA.class "smallButton", HA.style [ ( "background-color", "green" ), ( "color", "white" ) ] ]
            [ Html.text "Refresh" ]
        ]


toolSelector : Model -> Html Msg
toolSelector model =
    case model.appState.tool of
        TableOfContents ->
            tableOfContents model

        EditorTools ->
            editorTools model

        ReaderTools ->
            readerTools model


editorTools : Model -> Html Msg
editorTools model =
    div []
        [ searchOptionControl model
        , newDocument model
        ]


readerTools : Model -> Html Msg
readerTools model =
    div []
        [ searchOptionControl model
        , newDocument model
        ]


newDocument : Model -> Html Msg
newDocument model =
    button [ onClick NewDocument, HA.style [ ( "width", "180px" ), ( "margin-left", "10px" ), ( "background-color", "#444" ), ( "color", "white" ) ] ]
        [ Html.text "New Document" ]


tableOfContents : Model -> Html Msg
tableOfContents model =
    div []
        [ h4 [] [ Html.text ("Documents: " ++ (toString (List.length model.documents))) ]
        , documentListView model
        ]


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
