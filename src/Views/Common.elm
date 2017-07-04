module Views.Common exposing (documentListView, tool, publicCheckbox)

import StyleSheet exposing (..)
import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events exposing (..)
import Types exposing (..)
import Action.UI exposing (appStateWithPage)
import Views.Component as Component


tocStyle selectedDocument document =
    if selectedDocument == document then
        TOCItemSelected
    else
        TOCItem


viewTitle : Document -> Document -> Element Styles variation Msg
viewTitle selectedDocument document =
    el (tocStyle selectedDocument document)
        [ onClick (SelectDocument document)
        , paddingXY 4 4
        ]
        (text document.title)


documentListView : Model -> Element Styles variation Msg
documentListView model =
    column TOC
        [ yScrollbar, padding 20, spacing 5, width (px 300), height (px ((toFloat model.window.height) - 129.0)) ]
        ([ el Heading [ height (px 30), paddingXY 8 4 ] (text (Action.UI.numberOfDocuments model)) ]
            ++ (List.map (viewTitle model.current_document) model.documents)
        )


tool : Model -> Element Styles variation Msg
tool model =
    case model.appState.tool of
        TableOfContents ->
            documentListView model

        ReaderTools ->
            readerTools model

        EditorTools ->
            editorTools model

        DocumentParameterTools ->
            documentParameterTools model


searchOptionControl model =
    radio "Search domain"
        Radio
        [ verticalCenter, padding 20, spacing 20, width (px 300) ]
        [ option "My documents" (searchDomainChecked model Private) (el None [ onClick (UseSearchDomain Private) ] (text "My documents"))
        , option "Public documents" (searchDomainChecked model Public) (el None [ onClick (UseSearchDomain Public) ] (text "Public documents"))
        ]


searchDomainChecked : Model -> SearchDomain -> Bool
searchDomainChecked model domain =
    model.searchState.domain == domain


readerTools model =
    column TOC
        [ alignLeft, padding 20, spacing 10, width (px 300), height (px ((toFloat model.window.height) - 129.0)) ]
        [ el Box [ padding 20, center ] (text "Reader tools") ]


documentParameterTools model =
    column TOC
        [ alignLeft, padding 20, spacing 10, width (px 300), height (px ((toFloat model.window.height) - 129.0)) ]
        [ el Box [ padding 20, center ] (text "Document parameter tools") ]

-- editorTools : Model -> Model
editorTools model =
    column TOC
        [ alignLeft, padding 20, spacing 30, width (px 300), height (px ((toFloat model.window.height) - 129.0)) ]
        [ el Box [ width (px 250), height (px 35), paddingXY 10 10 ] (text "Editor tools")
        , column Zero
            [ spacing 4, height (px 130), alignLeft ]
            [ textArea Field
                [ yScrollbar
                , alignTop
                , width (px 250)
                , height (px 100)
                , onInput InputTags
                , placeholder "Keywords"
                ]
                (String.join ", " model.current_document.tags)
            , updateTagsButton model
            , el None [height (px 20)] (text "")
            , row None [padding 8, spacing 12] [Component.textFormatMenu model, Component.docTypeMenu model]
            ]
        ]


updateTagsButton model =
    el Button
        [ width (px 250)
        , alignBottom
        , onClick SaveCurrentDocument
        , height (px 25)
        , paddingXY 10 13
        ]
        (text "Update keywords")

publicCheckbox : Model -> Element Styles variation Msg
publicCheckbox model =
    row Box
        [ paddingXY 10 2, spacing 20, verticalCenter ]
        [ (node "input" <|
            el Zero
                [ onClick TogglePublic
                , Element.Attributes.checked model.current_document.attributes.public
                , width (px 18)
                , type_ "checkbox"
                ]
                (text "foo")
          )
        , (text "Public")
        ]
