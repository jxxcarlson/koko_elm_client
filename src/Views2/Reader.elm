module Views2.Reader exposing (..)

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


documentListView model =
    column TOC
        [ padding 10, spacing 10, width (px 300), height (px ((toFloat model.window.height) - 79.0)) ]
        (List.map (viewTitle model.current_document) model.documents)
