module Views.TOC
    exposing
        ( documentListView
        , documentListView0
        , documentListViewForPhone
        , documentStackView
        , toggleListView
        )

import Action.UI as UI
import Color
import Document.Stack as Stack
import Document.TOC
import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events exposing (..)
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
    -- select "searchMode" TOC [ width (px 120), EA.verticalCenter, on "change" (Json.map SelectSearchMode Json.string)]
    select "searchOrder"
        LightGray
        [ height (px 30), verticalCenter, onInput (SearchMsg << SelectSearchOrder) ]
        [ option "viewed" True (el Zero [ verticalCenter ] (text "Viewed"))
        , option "updated" False (el Zero [ verticalCenter ] (text "Updated"))
        , option "created" False (el Zero [ verticalCenter ] (text "Created"))
        , option "alpha" False (el Zero [ verticalCenter ] (text "Alpha"))
        ]


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
        [ yScrollbar, paddingTop 15, paddingLeft 15, spacing 0, height (px (toFloat (model.window.height - 170))) ]
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

        -- , searchOrderMenu model
        , documentStackView1 model
        ]


documentStackView1 : Model -> Element Styles variation Msg
documentStackView1 model =
    column DocumentStackColor
        [ yScrollbar, paddingTop 15, spacing 0, height (px (toFloat (model.window.height - 170))) ]
        (List.map (viewTitleInStack model model.current_document) (Stack.sorted model.documentStack))


documentIndicator : Document -> Model -> Element Styles variation Msg
documentIndicator document model =
    el Transparent [ height (px 25), moveDown 4, onClick ((DocMsg << SelectMaster) document) ] (documentIndicator1 document model)


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
            html (FontAwesome.caret_down Color.red 15)

        ( _, _, _ ) ->
            html (FontAwesome.caret_right Color.red 15)


childDocumentIndicator : Document -> Model -> Element style variation msg
childDocumentIndicator document model =
    case ( document.parent_id == 0, model.appState.masterDocLoaded, document.parent_id == model.master_document.id ) of
        ( True, _, _ ) ->
            html (FontAwesome.caret_right (Color.rgba 0 0 0 0) 15)

        ( False, True, True ) ->
            html (FontAwesome.caret_right (Color.rgba 0 0 0 0) 15)

        ( False, False, False ) ->
            html (FontAwesome.caret_up Color.blue 15)

        ( _, _, _ ) ->
            html (FontAwesome.caret_up Color.green 15)


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
    -12.0 + 16.0 * toFloat (level - 1)


documentListHeader : Model -> Element Styles variation Msg
documentListHeader model =
    el HeadingAlternate [ height (px 30), paddingXY 8 4 ] (text (UI.numberOfDocuments "Search results" model))


documentStackHeader : Model -> Element Styles variation Msg
documentStackHeader model =
    el HeadingAlternate [ height (px 30), paddingXY 8 4 ] (text (numberOfDocumentInStack model))


numberOfDocumentInStack : Model -> String
numberOfDocumentInStack model =
    "Recent documents" ++ ": " ++ toString (List.length model.documentStack)


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
            toFloat (model.window.width - 1300)

        scaledWindowDelta =
            (windowDelta / 30.0) |> round

        maxTitleCharacters =
            28 + scaledWindowDelta

        label =
            Document.TOC.tocLabel document

        titleText =
            Utility.shortString maxTitleCharacters document.title

        title =
            label ++ " " ++ titleText
    in
    el (tocStyle selectedDocument document)
        [ onClick ((DocMsg << SelectDocument) document)
        , paddingXY 0 0
        , height (px 20)
        ]
        (el TOCTitle [ verticalCenter ] (text title))


tocStyle : Document -> Document -> Styles
tocStyle selectedDocument document =
    if String.left 7 document.content == "Loading" then
        TOCItemLoading
    else if selectedDocument.id == document.id then
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
