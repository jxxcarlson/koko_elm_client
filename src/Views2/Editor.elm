module Views2.Editor exposing (..)

import StyleSheet exposing (..)
import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events exposing (onInput, onClick)
import Views2.Common as Common
import Color
import FontAwesome
import Views2.Component as Component
import Types exposing (..)
import Element.Keyed as Keyed
import Action.Document exposing (wordCount)
import Utility


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
        , named "TOCHeader"
            (Component.toolSelectorPanel model)
        , named "contentHeader"
            (inputText TitleStyle [ paddingXY 10 8, width (percent 100), height (percent 100), onInput Title, placeholder "Title" ] (model.current_document.title))
        , named "content"
            (Keyed.row None
                []
                [ ( (toString model.counter)
                  , (textArea None
                        [ width (percent 100)
                        , padding 8
                        , onInput InputContent
                        , Utility.onKeyUp DoRender
                        ]
                        (model.current_document.content)
                    )
                  )
                ]
            )
        , named "TOC" (Common.tool model)
        , named "footer" (Component.footer model)
        , named "editorPanel" (editorPanel model)
        ]
    ]


editorPanel model =
    row Panel
        [ paddingXY 10 6, spacing 15, center ]
        [ el Zero
            [ width (px 30)
            , onClick (NewDocument)
            , height (px 30)
            , padding 2
            , title "New document"
            ]
            (html (FontAwesome.plus Color.white 25))
        , el Zero
            [ width (px 30)
            , onClick (Refresh)
            , height (px 30)
            , padding 2
            , title "Refresh display & save. Also: press ESC"
            ]
            (html (FontAwesome.refresh Color.white 25))
        , full PanelInfo [ padding 11 ] (text ("Words: " ++ (toString <| wordCount <| model.current_document)))
        ]
