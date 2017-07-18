module Views.Common
    exposing
        ( documentListView
        , tool
        , publicCheckbox
        , recallLastSearchButton
        , printButton
        )

import StyleSheet exposing (..)
import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events exposing (..)
import Types exposing (..)
import Action.UI as UI
import Views.Component as Component
import FontAwesome
import Color


tocStyle : Document -> Document -> Styles
tocStyle selectedDocument document =
    if selectedDocument == document then
        if document.attributes.docType == "master" then
            TOCItemMasterSelected
        else
            TOCItemSelected
    else if document.attributes.docType == "master" then
        TOCItemMaster
    else
        TOCItem


viewTitle : Model -> Document -> Document -> Element Styles variation Msg
viewTitle model selectedDocument document =
    el (tocStyle selectedDocument document)
        [ onClick (SelectDocument document)
        , onDoubleClick (SelectMaster document)
        , paddingXY (documentIndentLevel document model) 4
        ]
        (text document.title)


documentIndentLevel : Document -> Model -> Float
documentIndentLevel document model =
    let
        level =
            if model.appState.masterDocLoaded then
                document.attributes.level
            else
                0
    in
        8.0 + 15.0 * (toFloat (level - 1))


viewTocItem : Child -> Element Styles variation Msg
viewTocItem child =
    el (None)
        [ paddingXY 4 4
        ]
        (text child.title)


documentListView : Model -> Element Styles variation Msg
documentListView model =
    column TOC
        [ yScrollbar, padding 20, spacing 5, width (px 300), height (px ((toFloat model.window.height) - 129.0)) ]
        ([ el Heading [ height (px 30), paddingXY 8 4 ] (text (UI.numberOfDocuments model)) ]
            ++ (List.map (viewTitle model model.current_document) model.documents)
        )


tableOfContents : Model -> Element Styles variation Msg
tableOfContents model =
    column TOC
        [ yScrollbar, padding 20, spacing 5, width (px 300), height (px ((toFloat model.window.height) - 129.0)) ]
        ([ el Heading [ height (px 30), paddingXY 8 4 ] (text (UI.tocNumberOfDocuments model)) ]
            ++ (List.map viewTocItem model.current_document.children)
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


searchOptionControl : Model -> Element Styles variation Msg
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


readerTools : Model -> Element Styles variation msg
readerTools model =
    column TOC
        [ alignLeft, padding 20, spacing 10, width (px 300), height (px ((toFloat model.window.height) - 129.0)) ]
        [ el Box [ padding 20, center ] (text "Reader tools") ]


documentParameterTools : Model -> Element Styles variation msg
documentParameterTools model =
    column TOC
        [ alignLeft, padding 20, spacing 10, width (px 300), height (px ((toFloat model.window.height) - 129.0)) ]
        [ el Box [ padding 20, center ] (text "Document parameter tools") ]


editorTools : Model -> Element Styles variation Msg
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
            , el None [ height (px 10) ] (text "")
            , el TOC [ height (px 25), width (px 200), paddingXY 8 12 ] (text ("Identifier: " ++ (UI.displayIdentifier model)))
            , el None [ height (px 0) ] (text "")
            , row TOC [ padding 8, spacing 12 ] [ Component.textFormatMenu model, Component.docTypeMenu model ]
            ]
        ]


updateTagsButton : Model -> Element Styles variation Msg
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


printButton : Model -> Element Styles variation Msg
printButton model =
    link ("http://localhost:4000/print/documents/" ++ (toString model.current_document.id)) <|
        el Zero [ verticalCenter ] (html (FontAwesome.print Color.white 25))


recallLastSearchButton : Model -> Element Styles variation Msg
recallLastSearchButton model =
    el Zero
        [ width (px 30)
        , onClick (RecallLastSearch)
        , height (px 30)
        , padding 2
        , title "Recall last search"
        ]
        (html (FontAwesome.rotate_left Color.white 25))
