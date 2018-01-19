module Views.Editor exposing (..)

import Action.Document exposing (wordCount)
import Color
import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events exposing (onClick, onInput)
import Element.Keyed as Keyed
import FontAwesome
import Http
import Request.Api
import StyleSheet exposing (..)
import Types exposing (..)
import Utility
import Views.Basic as Basic
import Views.Common as Common
import Views.Component as Component
import Views.Utility


editor : Model -> List (Element Styles variation Msg)
editor model =
    [ namedGrid Container
        { columns = [ fill 1, fill 3, fill 4 ]
        , rows =
            [ px 1 => [ spanAll "e_separator" ]
            , px 40 => [ span 1 "e_TOCHeader", span 1 "e_contentHeader", span 1 "e_editorPanel" ]
            , fill 1 => [ span 1 "e_TOC", span 1 "e_content", span 1 "e_renderedContent" ]
            ]
        }
        []
        [ --named "e_separator" (hairline Hairline)
          named "e_TOCHeader" (toolSelectorPanel model)
        , named "e_contentHeader" (titlePanel model)
        , named "e_content" (contentPanel model)
        , named "e_renderedContent" (Common.renderedContent model)
        , named "e_TOC" (Common.tool model)
        , named "e_editorPanel" (editorPanel model)
        ]
    ]



-- titlePanel : Model -> Element Styles variation Msg


titlePanel model =
    Keyed.row None
        []
        [ ( toString model.counter
          , inputText TitleStyle
                [ paddingXY 10 8
                , width (percent 100)
                , height (percent 100)
                , onInput (DocMsg << Title)
                , placeholder "Title"
                ]
                model.current_document.title
          )
        ]


contentPanel : Model -> Element Styles variation Msg
contentPanel model =
    Keyed.row None
        [ height (percent 100) ]
        [ ( toString model.counter
          , textArea Mono
                [ width (percent 100)
                , yScrollbar
                , padding 20
                , onInput (DocMsg << InputContent)
                , Utility.onKeyUp (DocMsg << DoRender)
                ]
                model.current_document.content
          )
        ]


editorPanel : Model -> Element Styles variation Msg
editorPanel model =
    row Panel
        [ paddingXY 10 6, spacing 15, center ]
        [ Common.publicCheckbox model
        , Views.Utility.visibleIf (model.current_document.attributes.textType == "latex") (typeSetAllButton model)
        , refreshButton model
        , notVisibleIfLatex model (toggleUpdateRateIndicator model)
        , notVisibleIfLatex model (toggleUpdateRateButton model)
        , full PanelInfo [] (el Zero [ verticalCenter ] (text ("ID: " ++ toString model.current_document.id)))
        , full PanelInfo [] (el Zero [ verticalCenter ] (text ("Words: " ++ (toString <| wordCount <| model.current_document))))
        , deleteButton model
        , Views.Utility.visibleIf (model.appState.deleteState == Pending) (deleteConfirmation model)
        ]


notVisibleIfLatex : Model -> Element Styles variation Msg -> Element Styles variation Msg
notVisibleIfLatex model body =
    -- notVisibleIf : Bool -> Element Styles variation Msg -> Element Styles variation Msg
    Views.Utility.notVisibleIf (model.current_document.attributes.textType == "latex") body


refreshButton : Model -> Element Styles variation Msg
refreshButton model =
    Basic.faIcon "Refresh display & save. Also: press ESC" FontAwesome.save [ onClick (DocMsg UpdateDocument) ]


typeSetAllButton : Model -> Element Styles variation Msg
typeSetAllButton model =
    Basic.faIcon2 "Typeset from scratch" FontAwesome.save [ onClick (DocMsg LatexFullRender) ]


toggleUpdateRateButton : Model -> Element Styles variation Msg
toggleUpdateRateButton model =
    Basic.faIcon "Toggle rate of update: very fast or very slow" (toggleUpdateRateIcon model) [ onClick (UIMsg ToggleUpdateRate) ]


toggleUpdateRateIndicator : Model -> Element Styles variation Msg
toggleUpdateRateIndicator model =
    if model.appState.tickerPaused then
        el Zero [ verticalCenter, title "Green = fast update, red = slow update" ] (html (FontAwesome.circle Color.red 25))
    else
        el Zero [ verticalCenter, title "Green = fast update, red = slow update" ] (html (FontAwesome.circle Color.green 25))


toggleUpdateRateIcon model =
    if model.appState.tickerPaused then
        FontAwesome.play
    else
        FontAwesome.pause


deleteButton : Model -> Element Styles variation Msg
deleteButton model =
    Basic.faIcon "Delete document" FontAwesome.trash [ onClick (DocMsg RequestDocumentDelete) ]


confirmDeleteButton : Model -> Element Styles variation Msg
confirmDeleteButton model =
    full PanelInfoRed [ padding 8, onClick (DocMsg DeleteCurrentDocument) ] (el Zero [ verticalCenter ] (text "Delete"))


cancelDeleteButton : Model -> Element Styles variation Msg
cancelDeleteButton model =
    full PanelInfoGreen [ padding 8, onClick (DocMsg CancelDocumentDelete) ] (el Zero [ verticalCenter ] (text "Cancel"))


deleteConfirmation : Model -> Element Styles variation Msg
deleteConfirmation model =
    row Panel
        [ spacing 10 ]
        [ cancelDeleteButton model
        , confirmDeleteButton model
        ]



-- , Common.searchOrderMenu model


toolSelectorPanel : Model -> Element Styles variation Msg
toolSelectorPanel model =
    row Panel
        [ paddingXY 10 6, spacing 12, justify ]
        [ Common.printButton model.current_document

        -- , exportButton model.current_document
        , exportButton2 model
        , renumberDocumentsButton model
        , imageCatalogueButton model.current_document
        , selectTableOfContents model
        , Common.toggleListView model
        , Common.recallLastSearchButton model
        , selectEditTools model
        ]


imageCatalogueButton : Document -> Element Styles variation Msg
imageCatalogueButton document =
    link (imageCatalogueUrl document) <|
        el Zero [ verticalCenter, target "_blank" ] (html (FontAwesome.image Color.white 25))


imageCatalogueUrl : Document -> String
imageCatalogueUrl document =
    Request.Api.imageCatalogueUrl ++ "/" ++ toString document.id


renumberDocumentsButton : Model -> Element Styles variation Msg
renumberDocumentsButton model =
    Basic.button "N" FlatButtonBlue [ onClick (DocMsg RenumberDocuments), width (px 30) ]


exportButton : Document -> Element Styles variation Msg
exportButton document =
    link (exportUrl document) <|
        el Zero [ verticalCenter, target "_blank" ] (html (FontAwesome.cloud_download Color.white 25))



-- exportButton : Document -> Element Styles variation Msg
-- exportButton document =
--     link (exportUrl document) <|
--         el Zero [ verticalCenter, target "_blank" ] (html (FontAwesome.cloud_download Color.white 25))
-- exportUrl : Document -> String
-- exportUrl document =
--     Request.Api.exportUrl ++ "/" ++ toString document.id ++ "?" ++ Common.printTypeString document


exportUrl : Document -> String
exportUrl document =
    Request.Api.exportUrl ++ "/" ++ toString document.id ++ "?" ++ Common.printTypeString document


exporterTextArea model =
    Keyed.row None
        [ height (px 0) ]
        [ ( toString model.counter
          , textArea Zero
                [ onInput (DocMsg << InputTextForExport)
                , Element.Attributes.value model.textToExport
                , height (px 0)
                , width (px 0)
                ]
                model.textToExport
          )
        ]


exportButton2 : Model -> Element Styles variation Msg
exportButton2 model =
    let
        prefix =
            Utility.compress "-" model.current_document.title

        fileName =
            prefix ++ ".tex"
    in
    link (dataUrl model.textToExport) <|
        el Zero [ verticalCenter, Element.Attributes.downloadAs fileName ] (html (FontAwesome.cloud_download Color.white 25))


dataUrl : String -> String
dataUrl data =
    "data:text/plain;charset=utf-8," ++ Http.encodeUri data


selectTableOfContents : Model -> Element Styles variation Msg
selectTableOfContents model =
    Basic.faIcon "Table of contents" FontAwesome.list [ onClick (UIMsg (SelectTool TableOfContents)) ]


selectEditTools : Model -> Element Styles variation Msg
selectEditTools model =
    el Zero
        [ width (px 85)
        , onClick (UIMsg (SelectTool EditorTools))
        , title "Tools"
        , height (px 30)
        , padding 2
        ]
        (html (FontAwesome.gear (Component.toolSelectorColor model EditorTools) 25))
