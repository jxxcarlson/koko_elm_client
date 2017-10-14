module Views.Common
    exposing
        ( exportButton
        , getDevice
        , imageCatalogueButton
        , tool
        , selectTableOfContents
        , publicCheckbox
        , recallLastSearchButton
        , printButton
        , homepage
        , getDocument
        , renderedContent
        , renderedContentForPhone
        , searchOrderMenu
        , specialContent
        , toggleListView
        )

import Action.UI as UI
import Color
import Configuration
import Document.Document as Document exposing (hasTag)
import Document.Stack as Stack
import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events exposing (..)
import Element.Keyed as Keyed
import FontAwesome
import Json.Encode
import Request.Api
import StyleSheet exposing (..)
import Types exposing (..)
import Views.Basic as Basic
import Views.Component as Component
import Views.TOC as TOC
import Views.Utility as Utility


renderedContent : Model -> Element Styles variation msg
renderedContent model =
    Keyed.row None [] [ ( (toString model.counter), innerRenderedContent model ) ]


innerRenderedContent : Model -> Element Styles variation msg
innerRenderedContent model =
    let
        h =
            (toFloat model.window.height) - 150
    in
        (el (contentStyle model)
            [ yScrollbar
            , id "rendered_text2"
            , paddingXY 50 50
            , width (percent 100)
            , height (px h)
            , property "innerHTML"
                (Json.Encode.string model.current_document.rendered_content)
            ]
            (text "")
        )


contentStyle : Model -> Styles
contentStyle model =
    let
        doc =
            model.current_document

        style_ =
            if Document.hasTag "background:dark" model.current_document then
                MainContentDark
            else
                MainContent
    in
        style_


renderedContentForPhone : Model -> Element Styles variation msg
renderedContentForPhone model =
    Keyed.row None [] [ ( (toString model.counter), innerRenderedContentForPhone model ) ]


innerRenderedContentForPhone : Model -> Element Styles variation msg
innerRenderedContentForPhone model =
    let
        h =
            (toFloat model.window.height) - 100
    in
        (el (contentStyle model)
            [ yScrollbar
            , id "rendered_text2"
            , paddingXY 50 50
            , width (percent 100)
            , height (px h)
            , property "innerHTML"
                (Json.Encode.string model.current_document.rendered_content)
            ]
            (text "")
        )


specialContent : Model -> Element Styles variation msg
specialContent model =
    Keyed.row None [] [ ( (toString model.counter), innerSpecialContent model ) ]


innerSpecialContent : Model -> Element Styles variation msg
innerSpecialContent model =
    let
        h =
            (toFloat model.window.height) - 180
    in
        (el Zero
            [ yScrollbar
            , id "rendered_text2"
            , padding 20
            , width (percent 100)
            , height (px h)
            , property "innerHTML"
                (Json.Encode.string model.specialDocument.rendered_content)
            ]
            (text "")
        )


selectTableOfContents : Model -> Element Styles variation Msg
selectTableOfContents model =
    Basic.faIcon "Table of contents" FontAwesome.list [ onClick (SelectTool TableOfContents) ]


toggleListView : Model -> Element Styles variation Msg
toggleListView model =
    Basic.faIcon "Home Page" FontAwesome.arrows_h [ onClick ToggleListView ]


tool : Model -> Element Styles variation Msg
tool model =
    case model.appState.tool of
        TableOfContents ->
            TOC.documentListView model

        ReaderTools ->
            readerTools model

        EditorTools ->
            editorTools model

        NewDocumentTools ->
            newDocumentTools model


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
        [ alignLeft, padding 20, spacing 10, height (px ((toFloat model.window.height) - 129.0)) ]
        [ el Box [ padding 20, center ] (text "Reader tools") ]


newDocumentTools : Model -> Element Styles variation Msg
newDocumentTools model =
    column TOC
        [ alignLeft, padding 20, spacing 15, width (px 300), height (px ((toFloat model.window.height) - 129.0)) ]
        [ el Box [ width (px 250), height (px 35), paddingXY 10 10 ] (text "New document tools")
        , column Zero
            [ spacing 15, height (px 130), alignLeft ]
            [ Utility.visibleIf model.appState.masterDocLoaded (newDocumentTable model)
            , Utility.visibleIf model.appState.masterDocLoaded (addToMasterDocumentButton model)
            , Utility.visibleIf model.appState.masterDocLoaded (selectAttachmentOption model)
            , row TOC [ padding 8, spacing 12 ] [ Component.textFormatMenu model, Component.docTypeMenu model ]
            ]
        ]


newDocumentTable : Model -> Element Styles variation Msg
newDocumentTable model =
    table TOC
        [ spacing 20, width (px 250) ]
        [ [ el TOCItemMaster [] (text "Master")
          , el TOCItemChild [] (text "Current")
          , (text "New")
          ]
        , [ el TOCItemMaster [] (text (Utility.shortString 20 model.master_document.title))
          , el TOCItemChild [] (text (Utility.shortString 20 (Stack.top 1 model.documentStack).title))
          , (text (Utility.shortString 20 model.current_document.title))
          ]
        ]


selectAttachmentOption : Model -> Element Styles variation Msg
selectAttachmentOption model =
    radio "Attach new document"
        TOC
        [ spacing 10, width (px 200), paddingXY 20 0 ]
        [ option "top" False (el TOC [ spacing 10, verticalCenter, onClick (AttachCurrentDocument "at-top") ] (text "At top"))
        , option "above" False (el TOC [ spacing 10, verticalCenter, onClick (AttachCurrentDocument "above") ] (text "Above current"))
        , option "below" False (el TOC [ spacing 10, verticalCenter, onClick (AttachCurrentDocument "below") ] (text "Below current"))
        , option "bottom" False (el TOC [ spacing 10, verticalCenter, onClick (AttachCurrentDocument "at-bottom") ] (text "At bottom"))
        ]


homepage : Model -> Element Styles variation Msg
homepage model =
    el FlatButtonBlue
        [ width (px 200)
        , onClick Types.UserHomePage
        , height (px 30)
        , paddingXY 8 0
        , verticalCenter
        ]
        (el Zero [ verticalCenter ] (text ("Home page")))


getDocument : Styles -> String -> String -> Model -> Element Styles variation Msg
getDocument style searchTerm label model =
    el style
        [ maxWidth (px 200)
        , onClick (Types.GetPublicPage searchTerm)
        , height (px 30)
        , paddingXY 8 0
        , verticalCenter
        ]
        (el Zero [ verticalCenter ] (text label))


editorTools : Model -> Element Styles variation Msg
editorTools model =
    column TOC
        [ alignLeft, padding 20, spacing 30, height (px ((toFloat model.window.height) - 129.0)) ]
        [ el Blue [ width (px 250), height (px 35), paddingXY 10 10 ] (text "Editor tools")
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
            , migrateFromADLButton model
            , el None [ height (px 10) ] (text "")
            , parentalControls model
            , el None [ height (px 10) ] (text "")
            , el Small [ height (px 25), width (px 200), paddingXY 8 12 ] (text ("Level: " ++ (toString model.current_document.attributes.level)))
            , el Small [ height (px 25), width (px 200), paddingXY 8 12 ] (text (UI.displayIdentifier model))
            , el None [ height (px 0) ] (text "")
            , row TOC [ padding 8, spacing 12 ] [ Component.textFormatMenu model, Component.docTypeMenu model ]
            ]
        ]


parentalControls : Model -> Element Styles variation Msg
parentalControls model =
    if model.current_document.attributes.docType == "master" then
        adoptChildrenButton model
    else
        parentIdPanel model


parentIdPanel : Model -> Element Styles variation Msg
parentIdPanel model =
    column Panel
        [ height (px 80), width (px 250) ]
        [ row Panel
            [ paddingXY 8 12 ]
            [ el Panel [ verticalCenter, paddingXY 8 0 ] (text "Parent: ")
            , parentIdPane model
            ]
        , el Panel [ verticalCenter, paddingXY 16 12 ] (text model.current_document.parent_title) -- model.current_document.parent_name
        ]


parentIdPane : Model -> Element Styles variation Msg
parentIdPane model =
    inputText Field
        [ onInput SetParentId
        , placeholder "parent_id"
        , paddingXY 5 0
        , height (px 25)
        , width (px 50)
        ]
        (toString model.current_document.parent_id)


adoptChildrenButton : Model -> Element Styles variation Msg
adoptChildrenButton model =
    Basic.button "Adopt children" FlatButtonBlue [ onClick AdoptChildren, width (px 250) ]


updateTagsButton : Model -> Element Styles variation Msg
updateTagsButton model =
    Basic.button "Update keywords" FlatButtonBlue [ onClick SaveCurrentDocument, width (px 250) ]


addToMasterDocumentButton : Model -> Element Styles variation Msg
addToMasterDocumentButton model =
    Basic.button "Add to master" FlatButton [ onClick AddToMasterDocument, width (px 250) ]


migrateFromADLButton : Model -> Element Styles variation Msg
migrateFromADLButton model =
    when (model.current_document.attributes.textType == "latex")
        (Basic.button "Migrate from Asciidoc-LaTeX" FlatButtonBlue [ onClick MigrateFromAsciidocLatex, width (px 250) ])


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


printButton : Document -> Element Styles variation Msg
printButton document =
    link (printUrl document) <|
        el Zero [ verticalCenter, target "_blank" ] (html (FontAwesome.print Color.white 25))


printUrl : Document -> String
printUrl document =
    Request.Api.printUrl ++ "/" ++ (toString document.id) ++ "?" ++ (printTypeString document)


exportButton : Document -> Element Styles variation Msg
exportButton document =
    link (exportUrl document) <|
        el Zero [ verticalCenter, target "_blank" ] (html (FontAwesome.cloud_download Color.white 25))


exportUrl : Document -> String
exportUrl document =
    Request.Api.exportUrl ++ "/" ++ (toString document.id) ++ "?" ++ (printTypeString document)


imageCatalogueUrl : Document -> String
imageCatalogueUrl document =
    Request.Api.imageCatalogueUrl ++ "/" ++ (toString document.id)


imageCatalogueButton : Document -> Element Styles variation Msg
imageCatalogueButton document =
    link (imageCatalogueUrl document) <|
        el Zero [ verticalCenter, target "_blank" ] (html (FontAwesome.image Color.white 25))


printTypeString : Document -> String
printTypeString document =
    case document.attributes.textType of
        "plain" ->
            "text=plain"

        "adoc" ->
            "text=adoc"

        "adoc:latex" ->
            "text=adoc_latex"

        "adoc_latex" ->
            "text=adoc_latex"

        "latex" ->
            "text=latex"

        "markdown" ->
            "text=markdown"

        _ ->
            "text=plain"


searchOrderMenu : Model -> Element Styles variation Msg
searchOrderMenu model =
    -- select "searchMode" TOC [ width (px 120), EA.verticalCenter, on "change" (Json.map SelectSearchMode Json.string)]
    select "searchOrder"
        LightGray
        [ height (px 25), verticalCenter, onInput SelectSearchOrder ]
        [ option "viewed" True (text "Viewed")
        , option "updated" False (text "Updated")
        , option "created" False (text "Created")
        , option "alpha" False (text "Alpha")
        ]


recallLastSearchButton : Model -> Element Styles variation Msg
recallLastSearchButton model =
    Basic.faIcon "Recall last search" FontAwesome.rotate_left [ onClick RecallLastSearch ]


getDevice : Int -> Types.Device
getDevice width =
    if width <= Configuration.phoneWidth then
        Phone
    else if width <= Configuration.tabletWidth then
        Tablet
    else
        Computer
