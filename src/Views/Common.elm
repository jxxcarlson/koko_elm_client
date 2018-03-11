module Views.Common
    exposing
        ( getDevice
        , getDocument
        , homepage
        , printButton
        , printTypeString
        , publicCheckbox
        , recallLastSearchButton
        , renderedContent
        , renderedContentForPhone
        , searchOrderMenu
        , specialContent
        , toggleListView
        , tool
        )

import Document.Access
import Array
import Color
import Configuration
import Document.Document as Document exposing (hasTag)
import Document.MasterDocument
import Document.Stack as Stack
import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events exposing (..)
import Element.Keyed as Keyed
import FontAwesome
import                                                                                                                            Json.Encode
import Request.Api
import StyleSheet exposing (..)
import Types
    exposing
        ( Device(..)
        , DocMsg(..)
        , Document
        , Model
        , Msg(..)
        , PageMsg(..)
        , SearchDomain(..)
        , SearchMsg(..)
        , Tool(..)
        , UIMsg(..)
        )
import Views.Basic as Basic
import Views.Component as Component
import Views.TOC as TOC
import Views.Utility as Utility


renderedContent : Model -> Element Styles variation msg
renderedContent model =
    Keyed.row None [] [ ( toString model.counter, innerRenderedContent model ) ]


innerRenderedContent : Model -> Element Styles variation msg
innerRenderedContent model =
    let
        h =
            toFloat model.window.height - 150
    in
    el (contentStyle model)
        [ yScrollbar
        , xScrollbar
        , id "rendered_text2"
        , paddingXY 50 50
        , width (percent 100)
        , height (px h)
        , property "innerHTML"
            (Json.Encode.string model.current_document.rendered_content)
        ]
        (text "")


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
    Keyed.row None [] [ ( toString model.counter, innerRenderedContentForPhone model ) ]


innerRenderedContentForPhone : Model -> Element Styles variation msg
innerRenderedContentForPhone model =
    let
        h =
            toFloat model.window.height - 100
    in
    el (contentStyle model)
        [ yScrollbar
        , id "rendered_text2"
        , paddingXY 50 50
        , width (percent 100)
        , height (px h)
        , property "innerHTML"
            (Json.Encode.string model.current_document.rendered_content)
        ]
        (text "")


specialContent : Model -> Element Styles variation msg
specialContent model =
    Keyed.row None [] [ ( toString model.counter, innerSpecialContent model ) ]


innerSpecialContent : Model -> Element Styles variation msg
innerSpecialContent model =
    let
        h =
            toFloat model.window.height - 180
    in
    el Zero
        [ yScrollbar
        , id "rendered_text2"
        , padding 20
        , width (percent 100)
        , height (px h)
        , property "innerHTML"
            (Json.Encode.string model.specialDocument.rendered_content)
        ]
        (text "")


toggleListView : Model -> Element Styles variation Msg
toggleListView model =
    Basic.faIcon "Home Page" FontAwesome.arrows_h [ onClick (UIMsg ToggleListView) ]


tool : Model -> Element Styles variation Msg
tool model =
    case model.appState.tool of
        TableOfContents ->
            TOC.documentListView model

        EditorTools ->
            editorTools model

        NewDocumentTools ->
            newDocumentTools model


newDocumentTools : Model -> Element Styles variation Msg
newDocumentTools model =
    column TOC
        [ alignLeft, padding 20, spacing 15, width (px 300), height (px (toFloat model.window.height - 129.0)) ]
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
          , text "New"
          ]
        , [ el TOCItemMaster [] (text (Utility.shortString 20 model.master_document.title))
          , el TOCItemChild [] (text (Utility.shortString 20 (Stack.top 1 model.documentStack).title))
          , text (Utility.shortString 20 model.current_document.title)
          ]
        ]


selectAttachmentOption : Model -> Element Styles variation Msg
selectAttachmentOption model =
    radio "Attach new document"
        TOC
        [ spacing 10, width (px 200), paddingXY 20 0 ]
        [ option "top" False (el TOC [ spacing 10, verticalCenter, onClick (DocMsg (AttachCurrentDocument "at-top")) ] (text "At top"))
        , option "above" False (el TOC [ spacing 10, verticalCenter, onClick (DocMsg (AttachCurrentDocument "above")) ] (text "Above current"))
        , option "below" False (el TOC [ spacing 10, verticalCenter, onClick (DocMsg (AttachCurrentDocument "below")) ] (text "Below current"))
        , option "bottom" False (el TOC [ spacing 10, verticalCenter, onClick (DocMsg (AttachCurrentDocument "at-bottom")) ] (text "At bottom"))
        ]


homepage : Model -> Element Styles variation Msg
homepage model =
    el FlatButtonBlue
        [ width (px 200)
        , onClick (PageMsg UserHomePage)
        , height (px 30)
        , paddingXY 8 0
        , verticalCenter
        ]
        (el Zero [ verticalCenter ] (text "Home page"))


getDocument : Styles -> String -> String -> Model -> Element Styles variation Msg
getDocument style searchTerm label model =
    el style
        [ maxWidth (px 200)
        , onClick (PageMsg (GetPublicPage searchTerm))
        , height (px 30)
        , paddingXY 8 0
        , verticalCenter
        ]
        (el Zero [ verticalCenter ] (text label))


editorTools : Model -> Element Styles variation Msg
editorTools model =
    column TOC
        [ alignLeft, yScrollbar, padding 20, spacing 10, height (px (toFloat model.window.height - 129.0)) ]
        [ el Blue [ width (px 250), height (px 35), paddingXY 10 10 ] (text "Editor tools")
        , column Zero
            [ spacing 5, height (px 130), alignLeft ]
            [ textArea Field
                [ yScrollbar
                , alignTop
                , width (px 250)
                , height (px 100)
                , onInput (DocMsg << InputTags)
                , placeholder "Keywords"
                ]
                (String.join ", " model.current_document.tags)
            , updateTagsButton model
            , el None [ height (px 10) ] (text "")
            , Utility.visibleIf (currentDocumentIsMaster model) (compileMasterButton model)
            , Utility.visibleIf (currentDocumentIsMaster model) (displayMasterDocumentWordCount model)
            , row TOC [ padding 8, spacing 12 , height (px 200) ] [ Component.textFormatMenu model, Component.docTypeMenu model ]
            , parentalControls model
            , el Small [ height (px 30), width (px 200), paddingLeft 8, paddingTop 16, paddingBottom 12 ] (text (displayIdentifier model))
            , versionPanel model
            , accessPanel model
        ]
        ]



fullAccessPanel model lines =
   column EditorPanel
        [ height (px 100), width (px 250), spacing 4 ]
        [ row EditorPanel [ paddingLeft 8, paddingTop 8][ el PanelSmallTypeHeading [ height (px 15),  paddingXY 8 8 ] (text "Shared with:")    ]
        , row EditorPanel [ paddingLeft 8,paddingLeft 8] [ column EditorPanel [spacing 4,  width (px 200), paddingXY 8 8] lines ]
        , row EditorPanel [paddingLeft 8, spacing 3] [shareDocumentButton model, shareDocumentPane model]
        ]

basicAccessPanel model =
   column Zero
        [ height (px 50), width (px 250), spacing 4, paddingXY 8 12]
        [  row Zero [spacing 3] [shareDocumentButton model, shareDocumentPane model] ]


accessPanel model = 
  let 
    lines = Document.Access.accessElementList model.current_document 
  in 
    if List.length lines > 0 then
      fullAccessPanel model lines
    else  
      basicAccessPanel model



levelDisplay : Model -> String
levelDisplay model = 
  "Level: " ++ (toString model.current_document.attributes.level)

versionPanel : Model -> Element Styles variation Msg
versionPanel model = 
   column EditorPanel
        [ height (px 100), width (px 250), spacing 8]
        [ row EditorPanel [ paddingLeft 8, paddingTop 16, paddingBottom 8 ][ el PanelSmallTypeHeading [ verticalCenter, paddingXY 8 8 ] (text (versionDisplay model)) ]
        , row EditorPanel [ paddingXY 16 0, spacing 12 ] [ newVersionButton model.current_document, showVersionsButton model.current_document]
        , row EditorPanel  [spacing 12, paddingTop 8, paddingBottom 8] [ el Small [height (px 25), width (px 72), paddingTop 12.0, paddingBottom 8.0, paddingLeft 16.0, paddingRight 8.0   ] (text "Repository:"), repositoryNamePane model]
        ]

repositoryNamePane : Model -> Element Styles variation Msg
repositoryNamePane model =
    inputText Field
        [ onInput (DocMsg << SetRepositoryName)
        , placeholder "repository"
        , paddingLeft 12.0  
        , height (px 25)
        , width (px 120)
        ]
        (Document.archiveName model model.current_document) 

versionDisplay : Model -> String 
versionDisplay model =
  "Version: " ++ (toString model.current_document.attributes.version)

repositoryDisplay : Model -> String 
repositoryDisplay model  =
  "Repository: " ++ Document.archiveName model model.current_document

newVersionButton : Document -> Element Styles variation Msg
newVersionButton document =
    link (newVersionUrl document) <|
        el FlatLinkBlue [ verticalCenter, target "_blank", onClick (DocMsg IncrementVersion)] (text "New version")


newVersionUrl : Document -> String
newVersionUrl document =
    Request.Api.newVersionUrl ++ "/" ++ toString document.id


showVersionsButton : Document -> Element Styles variation Msg
showVersionsButton document =
    link (showVersionsUrl document) <|
        el FlatLinkBlue [ verticalCenter, target "_blank" ] (text "Show versions")


showVersionsUrl : Document -> String
showVersionsUrl document =
    Request.Api.showVersionsUrl ++ "/" ++ toString document.id  

archiveDisplay : Model -> String 
archiveDisplay model =
  (versionDisplay model) ++ ", " ++ (levelDisplay model)


displayLine line =
    el Small [ height (px 25), width (px 200), paddingXY 8 12 ] (text line)


displayMasterDocumentWordCount : Model -> Element Styles variation msg
displayMasterDocumentWordCount model =
    displayLine (masterDocumentWordCount model)


currentDocumentIsMaster : Model -> Bool
currentDocumentIsMaster model =
    model.current_document.attributes.docType == "master"


masterDocumentWordCount : Model -> String
masterDocumentWordCount model =
    case currentDocumentIsMaster model of
        True ->
            wordCountDisplay <| model

        False ->
            ""


wordCountDisplay : Model -> String
wordCountDisplay model =
    let
        words =
            Document.MasterDocument.wordCount <| model

        pages =
            toString <| truncate <| toFloat words / 300.0
    in
    "Word count: " ++ toString words ++ " (" ++ pages ++ " pages)"


displayIdentifier : Model -> String
displayIdentifier model =
    let
        parts =
            String.split "." model.current_document.identifier |> Array.fromList

        datePart =
            Array.get 2 parts |> Maybe.withDefault "--"

        hashPart =
            Array.get 3 parts |> Maybe.withDefault "--"
    in
    datePart ++ "." ++ hashPart


parentalControls : Model -> Element Styles variation Msg
parentalControls model =
    if model.current_document.attributes.docType == "master" then
        adoptChildrenButton model
    else
        parentIdPanel model


parentIdPanel : Model -> Element Styles variation Msg
parentIdPanel model =
    column EditorPanel
        [ height (px 62), width (px 250) ]
        [ row EditorPanel
            [ paddingXY 8 10 ]
            [ el PanelSmallType [ verticalCenter, paddingXY 8 0 ] (text "Parent: ")
            , parentIdPane model,   el PanelSmallType [ verticalCenter, paddingXY 8 0 ] (text (levelDisplay model))
            ]
        , el PanelSmallType [ verticalCenter, paddingXY 16 8 ] (text model.current_document.parent_title) -- model.current_document.parent_name
        ]


parentIdPane : Model -> Element Styles variation Msg
parentIdPane model =
    inputText Field
        [ onInput (DocMsg << SetParentId)
        , placeholder "parent_id"
        , paddingXY 5 0
        , height (px 20)
        , width (px 50)
        ]
        (toString model.current_document.parent_id)


shareDocumentPane : Model -> Element Styles variation Msg
shareDocumentPane model =
    inputText Field
        [ onInput (DocMsg << InputShareDocumentCommand)
        , placeholder "username: rw"
        , paddingXY 5 0
        , height (px 30)
        , width (px 150)
        ]
        (model.appState.shareDocumentCommand)  
         

shareDocumentButton : Model -> Element Styles variation Msg
shareDocumentButton model =
    Basic.button "Share with:" FlatButtonBlue [ onClick (DocMsg UpdateShareData), width (px 85) ]
      

adoptChildrenButton : Model -> Element Styles variation Msg
adoptChildrenButton model =
    Basic.button "Adopt children" FlatButtonBlue [ onClick (DocMsg AdoptChildren), width (px 250) ]


compileMasterButton : Model -> Element Styles variation Msg
compileMasterButton model =
    Basic.button "Compile Master" FlatButtonBlue [ onClick (DocMsg CompileMaster), width (px 250) ]


updateTagsButton : Model -> Element Styles variation Msg
updateTagsButton model =
    Basic.button "Update keywords" FlatButtonBlue [ onClick (DocMsg SaveCurrentDocument), width (px 250) ]


addToMasterDocumentButton : Model -> Element Styles variation Msg
addToMasterDocumentButton model =
    Basic.button "Add to master" FlatButton [ onClick (DocMsg AddToMasterDocument), width (px 250) ]


migrateFromADLButton : Model -> Element Styles variation Msg
migrateFromADLButton model =
    when (model.current_document.attributes.textType == "latex")
        (Basic.button "Migrate from Asciidoc-LaTeX" FlatButtonBlue [ onClick (DocMsg MigrateFromAsciidocLatex), width (px 250) ])


publicCheckbox : Model -> Element Styles variation Msg
publicCheckbox model =
    row Box
        [ paddingXY 10 2, spacing 20, verticalCenter ]
        [ node "input" <|
            el Zero
                [ onClick (DocMsg TogglePublic)
                , Element.Attributes.checked model.current_document.attributes.public
                , width (px 18)
                , type_ "checkbox"
                ]
                (text "foo")
        , text "Public"
        ]


printButton : Document -> Element Styles variation Msg
printButton document =
    link (printUrl document) <|
        el Zero [ verticalCenter, target "_blank" ] (html (FontAwesome.print Color.white 25))


printUrl : Document -> String
printUrl document =
    Request.Api.printUrl ++ "/" ++ toString document.id ++ "?" ++ printTypeString document


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
        [ height (px 25), verticalCenter, onInput (SearchMsg << SelectSearchOrder) ]
        [ option "viewed" True (text "Viewed")
        , option "updated" False (text "Updated")
        , option "created" False (text "Created")
        , option "alpha" False (text "Alpha")
        ]


recallLastSearchButton : Model -> Element Styles variation Msg
recallLastSearchButton model =
    Basic.faIcon "Recall last search" FontAwesome.rotate_left [ onClick (SearchMsg RecallLastSearch) ]


getDevice : Int -> Types.Device
getDevice width =
    if width <= Configuration.phoneWidth then
        Phone
    else if width <= Configuration.tabletWidth then
        Tablet
    else
        Computer
