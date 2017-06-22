module Views2.Common exposing (documentListView, toolSelectorPanel, tool)

import Style exposing (..)
import StyleSheet exposing (..)
import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events exposing (..)
import Style exposing (..)
import Types exposing (..)
import Action.UI exposing (appStateWithPage)


tocStyle selectedDocument document =
    if selectedDocument == document then
        TOCItemSelected
    else
        TOCItem


viewTitle : Document -> Document -> Element Styles variation Msg
viewTitle selectedDocument document =
    el (tocStyle selectedDocument document)
        [ onClick (SelectDocument document)
        ]
        (text document.title)


documentListView : Model -> Element Styles variation Msg
documentListView model =
    column TOC
        [ padding 20, spacing 10, width (px 300), height (px ((toFloat model.window.height) - 129.0)) ]
        (List.map (viewTitle model.current_document) model.documents)


tool : Model -> Element Styles variation Msg
tool model =
    if model.appState.tool == TableOfContents then
        documentListView model
    else
        documentListView model


toolSelectorPanel model =
    row None
        [ spacing 8, height (px 44), paddingXY 10 7 ]
        [ el FlatButton [ onClick (SelectTool TableOfContents), alignBottom, height (px 30), padding 8 ] (text "TOC")
        , el FlatButton [ onClick (SelectTool EditorTools), alignBottom, height (px 30), padding 8 ] (text "Tools")
        , el FlatButton [ onClick Refresh, alignBottom, height (px 30), padding 8 ] (text "Refresh")
        ]
