module Views.TOC
    exposing
        ( documentListView
        , documentListViewForPhone
        , documentStackView
        , documentListView0
        , toggleListView
        )

import Action.UI as UI
import Document.Stack as Stack
import Color
import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events exposing (..)
import Element.Input as Input
import FontAwesome
import StyleSheet exposing (..)
import Types exposing (..)
import Views.Utility as Utility


documentListView : Model -> Element Styles variation Msg
documentListView model =
    case model.appState.activeDocumentList of
        SearchResultList ->
            documentListView0 model

        DocumentStackList ->
            documentStackView model


documentListViewForPhone : Model -> Element Styles variation Msg
documentListViewForPhone model =
    column None
        [ height (percent 100) ]
        [ documentListHeader model
        , documentListViewForPhone1 model
        ]


searchOrderMenu : Model -> Element Styles variation Msg
searchOrderMenu model =
    Input.select
        LightGray
        [ height (px 25), verticalCenter, onInput SelectSearchOrder ]
        { label = Input.labelAbove <| text "Search Order"
        , with = model.searchState.order
        , max = 4
        , options = []
        , menu =
            Input.menu None
                []
                [ Input.choice Viewed (text "Viewed")
                , Input.choice Updated (text "Updated")
                , Input.choice Created (text "Created")
                , Input.choice Alphabetical (text "Alpha")
                ]
        }


documentListView0 : Model -> Element Styles variation Msg
documentListView0 model =
    column None
        [ height (percent 100), paddingBottom 20 ]
        [ documentListHeader model
        , searchOrderMenu model
        , documentListView1 model
        ]


documentListView1 : Model -> Element Styles variation Msg
documentListView1 model =
    column PaleBlue
        [ yScrollbar, paddingTop 15, paddingLeft 15, spacing 0, height (px (toFloat (model.window.height - 140))) ]
        (List.map (viewTitle model model.current_document) model.documents)


documentListViewForPhone1 : Model -> Element Styles variation Msg
documentListViewForPhone1 model =
    column PaleBlue
        [ yScrollbar
        , width (px (toFloat model.window.width))
        , spacing 0
        , height (px (toFloat (model.window.height - 90)))
        , paddingLeft 10
        , paddingTop 10
        ]
        (List.map (viewTitle model model.current_document) model.documents)


documentStackView : Model -> Element Styles variation Msg
documentStackView model =
    column None
        [ height (percent 100), minWidth (px 200) ]
        [ documentStackHeader model
        , searchOrderMenu model
        , documentStackView1 model
        ]


documentStackView1 : Model -> Element Styles variation Msg
documentStackView1 model =
    column PaleBlue2
        [ yScrollbar, paddingTop 15, spacing 0, height (px (toFloat (model.window.height - 140))) ]
        (List.map (viewTitleInStack model model.current_document) (Stack.sorted model.documentStack))


documentIndicator : Document -> Model -> Element Styles variation Msg
documentIndicator document model =
    el Transparent [ height (px 25), (moveDown 4), onClick (SelectMaster document) ] (documentIndicator1 document model)


documentIndicator1 : Document -> Model -> Element style variation msg
documentIndicator1 document model =
    if document.attributes.docType == "master" then
        masterDocumentIndicator document model
    else
        childDocumentIndicator document model


masterDocumentIndicator : Document -> Model -> Element style variation msg
masterDocumentIndicator document model =
    case ( model.appState.masterDocLoaded, model.appState.masterDocOpened, document.id == model.master_document.id ) of
        ( True, True, True ) ->
            (html (FontAwesome.caret_down Color.red 15))

        ( _, _, _ ) ->
            (html (FontAwesome.caret_right Color.red 15))


childDocumentIndicator : Document -> Model -> Element style variation msg
childDocumentIndicator document model =
    case ( document.parent_id == 0, model.appState.masterDocLoaded, document.parent_id == model.master_document.id ) of
        ( True, _, _ ) ->
            (html (FontAwesome.caret_right (Color.rgba 0 0 0 0) 15))

        ( False, True, True ) ->
            (html (FontAwesome.caret_right (Color.rgba 0 0 0 0) 15))

        ( False, False, False ) ->
            (html (FontAwesome.caret_up (Color.blue) 15))

        ( _, _, _ ) ->
            (html (FontAwesome.caret_up (Color.green) 15))


documentIndentLevel : Document -> Model -> Float
documentIndentLevel document model =
    let
        level =
            case ( model.appState.masterDocLoaded, document.parent_id == model.master_document.id ) of
                ( True, True ) ->
                    document.attributes.level

                ( _, _ ) ->
                    1
    in
        -12.0 + 16.0 * (toFloat (level - 1))


documentListHeader : Model -> Element Styles variation Msg
documentListHeader model =
    el HeadingAlternate [ height (px 30), paddingXY 8 4 ] (text (UI.numberOfDocuments "Search results" model))


documentStackHeader : Model -> Element Styles variation Msg
documentStackHeader model =
    el HeadingAlternate [ height (px 30), paddingXY 8 4 ] (text (numberOfDocumentInStack model))


numberOfDocumentInStack : Model -> String
numberOfDocumentInStack model =
    "Recent documents" ++ ": " ++ (toString (List.length model.documentStack))


viewTitle : Model -> Document -> Document -> Element Styles variation Msg
viewTitle model selectedDocument document =
    row Zero
        [ verticalCenter, paddingXY (documentIndentLevel document model) 0 ]
        [ documentIndicator document model
        , titleDisplay model selectedDocument document
        ]


viewTitleInStack : Model -> Document -> Document -> Element Styles variation Msg
viewTitleInStack model selectedDocument document =
    row Zero
        [ verticalCenter, paddingXY 4 4 ]
        [ documentIndicator document model
        , titleDisplay model selectedDocument document
        ]


titleDisplay : Model -> Document -> Document -> Element Styles variation Msg
titleDisplay model selectedDocument document =
    let
        windowDelta =
            (toFloat (model.window.width - 1300))

        scaledWindowDelta =
            ((windowDelta / 25.0) |> round)

        maxTitleCharacters =
            (35 + scaledWindowDelta)
    in
        el (tocStyle selectedDocument document)
            [ onClick (SelectDocument document)
            , paddingXY 8 0
            , height (px 20)
            ]
            (el TOCTitle [ verticalCenter ] (text (Utility.shortString maxTitleCharacters document.title)))


tocStyle : Document -> Document -> Styles
tocStyle selectedDocument document =
    if selectedDocument.id == document.id then
        if document.attributes.docType == "master" then
            TOCItemMasterSelected
        else if document.parent_id /= 0 then
            TOCItemChildSelected
        else
            TOCItemSelected
    else if document.attributes.docType == "master" then
        TOCItemMaster
    else if document.parent_id /= 0 then
        TOCItemChild
    else
        TOCItem


toggleListView : Model -> ( Model, Cmd Msg )
toggleListView model =
    let
        newActiveDocumentList =
            case model.appState.activeDocumentList of
                SearchResultList ->
                    DocumentStackList

                DocumentStackList ->
                    SearchResultList

        appState =
            model.appState

        newAppState =
            { appState
                | activeDocumentList = newActiveDocumentList
                , tool = TableOfContents
            }
    in
        ( { model | appState = newAppState }, Cmd.none )
