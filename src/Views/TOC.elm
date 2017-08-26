module Views.TOC exposing(documentListView, documentStackView, documentListView0, toggleListView)

import Action.Document
import Action.UI as UI
import Color
import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events exposing (..)
import Element.Keyed as Keyed
import FontAwesome
import Json.Encode
import Request.Api
import String.Extra
import StyleSheet exposing (..)
import Types exposing (..)
import Views.Basic as Basic
import Views.Component as Component
import Views.Utility as Utility

-- tableOfContents : Model -> Element Styles variation Msg
-- tableOfContents model =
--     column TOC
--         [ yScrollbar, padding 20, spacing 5, height (px ((toFloat model.window.height) - 129.0)) ]
--         ([ el Heading [ height (px 30), paddingXY 8 4 ] (text (UI.tocNumberOfDocuments model)) ]
--             ++ (List.map viewTocItem model.current_document.children)
--         )

documentListView : Model -> Element Styles variation Msg
documentListView model =
  case model.appState.activeDocumentList of
    SearchResultList -> documentListView0 model
    DocumentStackList -> documentStackView model

documentListView0 : Model -> Element Styles variation Msg
documentListView0 model =
    column None [height (percent 100)] [
         documentListHeader model
         ,documentListView1 model
       ]

documentListView1 model =
   column PaleBlue [ yScrollbar, paddingTop 15, spacing 0, height (px (toFloat (model.window.height - 140))) ]
    (List.map (viewTitle model model.current_document) model.documents)

documentStackView : Model -> Element Styles variation Msg
documentStackView model =
    column None [height (percent 100), minWidth (px 200)] [
         documentStackHeader model
         ,documentStackView1 model
       ]

documentStackView1 model =
   column PaleBlue [ yScrollbar, paddingTop 15, spacing 0, height (px (toFloat (model.window.height - 140))) ]
    (List.map (viewTitle model model.current_document) model.documentStack)

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
    row Zero [ verticalCenter, paddingXY (documentIndentLevel document model) 4 ] [
     documentIndicator document model
     , titleDisplay model selectedDocument document
  ]

titleDisplay model selectedDocument document =
  el (tocStyle selectedDocument document)
      [ onClick (SelectDocument document)
      , onDoubleClick (SelectMaster document)
      , paddingXY 8 0
      , height (px 20)
      ]
      (el None [verticalCenter] (text (Utility.shortString 30 document.title)))


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

toggleListView model =
  let
    newActiveDocumentList = case model.appState.activeDocumentList of
      SearchResultList -> DocumentStackList
      DocumentStackList -> SearchResultList
    appState = model.appState
    masterDocLoaded_ = if newActiveDocumentList == DocumentStackList then
                        False
                      else
                        True
    newAppState =  { appState
     | activeDocumentList = newActiveDocumentList
       , masterDocLoaded = masterDocLoaded_
       , tool = TableOfContents }
  in
    ({model | appState = newAppState}, Cmd.none)
