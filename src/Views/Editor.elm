module Views.Editor exposing (..)

import StyleSheet exposing (..)
import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events exposing (onInput, onClick)
import Views.Basic as Basic
import Views.Common as Common
import Color
import FontAwesome
import Views.Component as Component
import Types exposing (..)
import Element.Keyed as Keyed
import Action.Document exposing (wordCount)
import Utility
import Types exposing (..)
import FontAwesome
import Views.Common as Common


editor : Model -> List (Element Styles variation Msg)
editor model =
    [ namedGrid Container
        { columns = [ fill 1, fill 3, fill 3 ]
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


titlePanel : Model -> Element Styles variation Msg
titlePanel model =
    (inputText TitleStyle [ paddingXY 10 8, width (percent 100), height (percent 100), onInput Title, placeholder "Title" ] (model.current_document.title))


contentPanel : Model -> Element Styles variation Msg
contentPanel model =
    (Keyed.row None
        [ height (percent 100) ]
        [ ( (toString model.counter)
          , (textArea Mono
                [ width (percent 100)
                , yScrollbar
                , padding 20
                , onInput InputContent
                , Utility.onKeyUp DoRender
                ]
                (model.current_document.content)
            )
          )
        ]
    )


editorPanel : Model -> Element Styles variation Msg
editorPanel model =
    row Panel
        [ paddingXY 10 6, spacing 15, center ]
        [ Common.publicCheckbox model
        , refreshButton model
        , toggleUpdateRateIndicator model
        , toggleUpdateRateButton model
        , full PanelInfo [] (el Zero [ verticalCenter ] (text ("ID: " ++ (toString model.current_document.id))))
        , full PanelInfo [] (el Zero [ verticalCenter ] (text ("Words: " ++ (toString <| wordCount <| model.current_document))))
        , deleteButton model
        ]


refreshButton : Model -> Element Styles variation Msg
refreshButton model =
    Basic.faIcon "Refresh display & save. Also: press ESC" FontAwesome.save [ onClick (UpdateDocument) ]


toggleUpdateRateButton : Model -> Element Styles variation Msg
toggleUpdateRateButton model =
    Basic.faIcon "Toggle rate of update: very fast or very slow" (toggleUpdateRateIcon model) [ onClick (ToggleUpdateRate) ]


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
    Basic.faIcon "Delete document" FontAwesome.trash [ onClick (DeleteCurrentDocument) ]



-- , Common.searchOrderMenu model


toolSelectorPanel : Model -> Element Styles variation Msg
toolSelectorPanel model =
    row Panel
        [ paddingXY 10 6, spacing 12, justify ]
        [ Common.printButton model.current_document

        --, Utility.visibleIf (model.current_document.attributes.textType == "latex") (Common.exportButton model.current_document)
        , Common.exportButton model.current_document
        , Common.imageCatalogueButton model.current_document
        , Common.selectTableOfContents model
        , Common.toggleListView model
        , Common.recallLastSearchButton model
        , selectEditTools model
        ]


selectEditTools : Model -> Element Styles variation Msg
selectEditTools model =
    el Zero
        [ width (px 85)
        , onClick (SelectTool EditorTools)
        , title "Tools"
        , height (px 30)
        , padding 2
        ]
        (html (FontAwesome.gear (Component.toolSelectorColor model EditorTools) 25))
