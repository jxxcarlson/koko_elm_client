module Views.Editor exposing (..)

import StyleSheet exposing (..)
import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events exposing (onInput, onClick)
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
        { columns = [ px 300, fill 1, fill 1 ]
        , rows =
            [ px 1 => [ spanAll "separator" ]
            , px 40 => [ span 1 "TOCHeader", span 1 "contentHeader", span 1 "editorPanel" ]
            , px 650 => [ span 1 "TOC", span 1 "content", span 1 "sidebar" ]
            , px 40 => [ spanAll "footer" ]
            ]
        }
        []
        [ named "separator" (hairline Hairline)
        , named "TOCHeader" (toolSelectorPanel model)
        , named "contentHeader" (titlePanel model)
        , named "content" (contentPanel model)
        , named "TOC" (Common.tool model)
        , named "footer" (Component.footer model)
        , named "editorPanel" (editorPanel model)
        ]
    ]


titlePanel : Model -> Element Styles variation Msg
titlePanel model =
    (inputText TitleStyle [ paddingXY 10 8, width (percent 100), height (percent 100), onInput Title, placeholder "Title" ] (model.current_document.title))


contentPanel : Model -> Element Styles variation Msg
contentPanel model =
    (Keyed.row None
        []
        [ ( (toString model.counter)
          , (textArea None
                [ width (percent 100)
                , yScrollbar
                , padding 8
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
        , newDocumentButton model
        , refreshButton model
        , full PanelInfo [] ( el Zero [verticalCenter] (text ("ID: " ++ (toString model.current_document.id))))
        , full PanelInfo [] (el Zero [verticalCenter] (text ("Words: " ++ (toString <| wordCount <| model.current_document))))
        , deleteButton model
        ]


newDocumentButton : Model -> Element Styles variation Msg
newDocumentButton model =
    el Zero
        [ width (px 30)
        , onClick (NewDocument)
        , height (px 30)
        , padding 2
        , title "New document"
        ]
        (html (FontAwesome.plus Color.white 25))


refreshButton : Model -> Element Styles variation Msg
refreshButton model =
    el Zero
        [ width (px 30)
        , onClick (Refresh)
        , height (px 30)
        , padding 2
        , title "Refresh display & save. Also: press ESC"
        ]
        (html (FontAwesome.refresh Color.white 25))


deleteButton : Model -> Element Styles variation Msg
deleteButton model =
    el Zero
        [ width (px 30)
        , onClick (DeleteCurrentDocument)
        , height (px 30)
        , padding 2
        , title "Delete document"
        ]
        (html (FontAwesome.trash Color.white 25))


toolSelectorPanel : Model -> Element Styles variation Msg
toolSelectorPanel model =
    row Panel
        [ paddingXY 10 6, spacing 15, center ]
        [ Common.selectTableOfContents model
        , Common.printButton model.current_document
        , selectEditTools model
        , Common.recallLastSearchButton model
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
