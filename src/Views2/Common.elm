module Views2.Common exposing (documentListView, toolSelectorPanel, tool)

import Style exposing (..)
import StyleSheet exposing (..)
import Color
import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events exposing (..)
import Style exposing (..)
import Types exposing (..)
import Action.UI exposing (appStateWithPage)
import FontAwesome


tocStyle selectedDocument document =
    if selectedDocument == document then
        TOCItemSelected
    else
        TOCItem


viewTitle : Document -> Document -> Element Styles variation Msg
viewTitle selectedDocument document =
    el (tocStyle selectedDocument document)
        [ onClick (SelectDocument document)
        , paddingXY 4 4
        ]
        (text document.title)


documentListView : Model -> Element Styles variation Msg
documentListView model =
    column TOC
        [ padding 20, spacing 5, width (px 300), height (px ((toFloat model.window.height) - 129.0)) ]
        ([ (text ("Documents: " ++ (toString (List.length model.documents)))) ]
            ++ (List.map (viewTitle model.current_document) model.documents)
        )


tool : Model -> Element Styles variation Msg
tool model =
    case model.appState.tool of
        TableOfContents ->
            documentListView model

        ReaderTools ->
            basicTools model

        EditorTools ->
            basicTools model


toolSelectorPanel model =
    row Panel
        [ spacing 8, height (px 44), paddingXY 10 7 ]
        [ el FlatButton [ onClick (SelectTool TableOfContents), alignBottom, height (px 30), padding 8 ] (text "TOC")
        , el FlatButton [ onClick (SelectTool EditorTools), alignBottom, height (px 30), padding 8 ] (text "Tools")
        , el FlatButton [ onClick Refresh, alignBottom, height (px 30), padding 8 ] (text "Refresh")
        ]


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


basicTools model =
    column TOC
        [ alignLeft, spacing 10, width (px 300), height (px ((toFloat model.window.height) - 129.0)) ]
        [ searchOptionControl model ]
