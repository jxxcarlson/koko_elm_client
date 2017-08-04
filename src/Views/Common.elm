module Views.Common
    exposing
        ( documentListView
        , tool
        , selectTableOfContents
        , tableOfContents
        , publicCheckbox
        , recallLastSearchButton
        , printButton
        , visibleIf
        , notVisibleIf
        , homepage
        , getDocument
        , renderedContent
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
import Request.Api
import Action.Document
import String.Extra

import Json.Encode


renderedContent model =
  let
    h = (toFloat model.window.height) - 150
  in
    (el Zero [yScrollbar, id "rendered_text2", padding 20, maxWidth (px 550), width (percent 100), height (px h), property "innerHTML"
       (Json.Encode.string model.current_document.rendered_content)] (text ""))


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


viewTitle : Model -> Document -> Document -> Element Styles variation Msg
viewTitle model selectedDocument document =
    row Zero [ verticalCenter, paddingXY (documentIndentLevel document model) 4 ] [
     documentIndicator document model
     , titleDisplay model selectedDocument document
  ]

titleDisplay model selectedDocument document =
  el (tocStyle selectedDocument document)
      [ onClick (SelectDocument document)
      , onDoubleClick (SelectMaster document)
      , paddingXY 8 0
      , height (px 30)
      ]
      (el None [moveDown 15.0] (text (shortString 30 document.title)))


shortString : Int -> String -> String
shortString nChars str =
  let
    parts =  String.Extra.softBreak nChars str
  in
    if List.length parts > 1 then
       parts |> List.head |> Maybe.withDefault "" |> \str -> str ++ " ..."
    else
       str


documentIndicator document model =
  el PaleBlue [ height (px 25)] (documentIndicator1 document model)

documentIndicator1 document model =
  if (document.attributes.docType == "master") then
    if model.appState.masterDocLoaded then
      (html (FontAwesome.caret_down Color.red 15))
    else
      (html (FontAwesome.caret_right Color.red 15))
  else if document.parent_id > 0 &&  model.appState.masterDocLoaded == False then
       (html (FontAwesome.caret_up Color.blue 15))
  else
    (html (FontAwesome.caret_right (Color.rgba 0 0 0 0) 15))

documentIndentLevel : Document -> Model -> Float
documentIndentLevel document model =
    let
        level =
            if model.appState.masterDocLoaded then
                document.attributes.level
            else
                1
    in
        8.0 + 15.0 * (toFloat (level - 1))


viewTocItem : Child -> Element Styles variation Msg
viewTocItem child =
    el (None)
        [ paddingXY 4 4
        ]
        (text child.title)


documentListView : String -> Model -> Element Styles variation Msg
documentListView title model =
    column None [height (percent 100)] [
         documentListHeader title model
         ,documentListView1 title model
       ]


documentListView1 title model =
   column PaleBlue [ yScrollbar, paddingTop 15, spacing 0, width (px 300), height (px (toFloat (model.window.height - 200))) ]
    (List.map (viewTitle model model.current_document) model.documents)


documentListHeader : String -> Model -> Element Styles variation Msg
documentListHeader title model =
  el HeadingAlternate [ height (px 30), paddingXY 8 4 ] (text (UI.numberOfDocuments title model))


selectTableOfContents : Model -> Element Styles variation Msg
selectTableOfContents model =
    el Zero
        [ width (px 85)
        , onClick (SelectTool TableOfContents)
        , title "Table of contents"
        , height (px 30)
        , padding 2
        ]
        (html (FontAwesome.list (Component.toolSelectorColor model TableOfContents) 25))

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
            documentListView "Documents" model

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
        [ alignLeft, padding 20, spacing 10, width (px 300), height (px ((toFloat model.window.height) - 129.0)) ]
        [ el Box [ padding 20, center ] (text "Reader tools") ]


newDocumentTools : Model -> Element Styles variation Msg
newDocumentTools model =
    column TOC
        [ alignLeft, padding 20, spacing 15, width (px 300), height (px ((toFloat model.window.height) - 129.0)) ]
        [ el Box [ width (px 250), height (px 35), paddingXY 10 10 ] (text "New document tools")
        , column Zero
            [ spacing 15, height (px 130), alignLeft ]
            [
              visibleIf model.appState.masterDocLoaded (newDocumentTable model)
            , visibleIf model.appState.masterDocLoaded (addToMasterDocumentButton model)
            , visibleIf model.appState.masterDocLoaded (selectAttachmentOption model)
            , row TOC [ padding 8, spacing 12 ] [ Component.textFormatMenu model, Component.docTypeMenu model ]
            ]
        ]

newDocumentTable : Model -> Element Styles variation Msg
newDocumentTable model =
  table TOC [spacing 20, width (px 250)]
    [
      [
        el TOCItemMaster [] (text "Master"),
        el TOCItemChild [](text "Current"),
        (text "New")

      ]
      ,[el TOCItemMaster [] (text (softTruncate 20 model.master_document.title))
         ,el TOCItemChild [] (text (softTruncate 20 (Action.Document.docStackTop model.documentStack).title))
         ,(text (softTruncate 20 model.current_document.title))
      ]
    ]

selectAttachmentOption : Model -> Element Styles variation Msg
selectAttachmentOption model =
  radio "Attach new document" TOC [spacing 10, width (px 200), paddingXY 20 0]
    [ option "top" False (el TOC [spacing 10, verticalCenter, onClick (AttachCurrentDocument "at-top") ] (text "At top"))
    , option "above" False (el TOC [spacing 10, verticalCenter , onClick (AttachCurrentDocument "above") ] (text "Above current"))
    , option "below" False (el TOC [spacing 10, verticalCenter, onClick (AttachCurrentDocument "below")] (text "Below current"))
    , option "bottom" False (el TOC [spacing 10, verticalCenter, onClick (AttachCurrentDocument "at-bottom") ] (text "At bottom"))
    ]

softTruncate : Int -> String -> String
softTruncate k str =
  String.Extra.softBreak k str |> List.head |> Maybe.withDefault "--"

addToMasterDocumentButton : Model -> Element Styles variation Msg
addToMasterDocumentButton model =
  el FlatButton
      [ width (px 200)
      , onClick AddToMasterDocument
      , height (px 30)
      , paddingXY 8 0
      , verticalCenter
      ]
      (el Zero [verticalCenter] (text ("Add to master")))

homepage : Model -> Element Styles variation Msg
homepage model =
  el FlatButtonBlue
      [ width (px 200)
      , onClick Types.UserHomePage
      , height (px 30)
      , paddingXY 8 0
      , verticalCenter
      ]
      (el Zero [verticalCenter] (text ("Home page")))

getDocument : Styles -> String -> String -> Model -> Element Styles variation Msg
getDocument style searchTerm label model =
  el style
      [ width (px 200)
      , onClick (Types.GetPublicPage searchTerm)
      , height (px 30)
      , paddingXY 8 0
      , verticalCenter
      ]
      (el Zero [verticalCenter] (text label))

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
            , parentalControls model
            , el None [ height (px 10) ] (text "")
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
  column Panel [ height (px 80) ,width (px 250)] [
    row Panel [paddingXY 8 12 ] [
      el Panel [verticalCenter, paddingXY 8 0] (text "Parent: ")
      , parentIdPane model
    ]
    , el Panel [verticalCenter, paddingXY 16 12] (text model.current_document.parent_title) -- model.current_document.parent_name
  ]

parentIdPane : Model -> Element Styles variation Msg
parentIdPane model =
  inputText Field [
     onInput SetParentId,
     placeholder "parent_id" ,
     paddingXY 5 0,
     height (px 25),
     width (px 50)]
  (toString model.current_document.parent_id)

adoptChildrenButton : Model -> Element Styles variation Msg
adoptChildrenButton model =
    el Button
        [ width (px 250)
        , alignBottom
        , onClick AdoptChildren
        , height (px 25)
        , paddingXY 10 13
        ]
        (text "Adopt children")


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


printButton : Document -> Element Styles variation Msg
printButton document =
    link (printUrl document) <|
        el Zero [ verticalCenter, target "_blank"] (html (FontAwesome.print Color.white 25))

printUrl : Document -> String
printUrl document =
    Request.Api.printUrl ++ "/" ++ (toString document.id) ++ "?" ++ (printTypeString document)

printTypeString : Document -> String
printTypeString document =
  case document.attributes.textType of
    "plain" -> "text=plain"
    "adoc" -> "text=adoc"
    "adoc:latex" -> "text=adoc:latex"
    "latex" -> "text=latex"
    "markdown" -> "text=markdown"
    _ -> "text=plain"


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

visibleIf : Bool -> Element Styles variation Msg -> Element Styles variation Msg
visibleIf condition body =
    if condition then
        body
    else
        el None [] (text "")

notVisibleIf : Bool -> Element Styles variation Msg -> Element Styles variation Msg
notVisibleIf condition body =
    if (not condition)then
        body
    else
        el None [] (text "")
