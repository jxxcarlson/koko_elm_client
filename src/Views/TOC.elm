module Views.TOC exposing(..)

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

tableOfContents : Model -> Element Styles variation Msg
tableOfContents model =
    column TOC
        [ yScrollbar, padding 20, spacing 5, width (px 300), height (px ((toFloat model.window.height) - 129.0)) ]
        ([ el Heading [ height (px 30), paddingXY 8 4 ] (text (UI.tocNumberOfDocuments model)) ]
            ++ (List.map viewTocItem model.current_document.children)
        )


documentListView : String -> Model -> Element Styles variation Msg
documentListView title model =
    column None [height (percent 100)] [
         documentListHeader title model
         ,documentListView1 title model
       ]


documentListView1 title model =
   column PaleBlue [ yScrollbar, paddingTop 15, spacing 0, width (px 300), height (px (toFloat (model.window.height - 200))) ]
    (List.map (viewTitle model model.current_document) model.documents)

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




documentListHeader : String -> Model -> Element Styles variation Msg
documentListHeader title model =
  el HeadingAlternate [ height (px 30), paddingXY 8 4 ] (text (UI.numberOfDocuments title model))


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
      (el None [moveDown 15.0] (text (Utility.shortString 30 document.title)))


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
