module Views.Reader exposing (..)

import StyleSheet exposing (..)
import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Attributes exposing (..)
import Element.Events exposing (onClick)
import Views.Common as Common
import Views.Component as Component
import Types exposing (..)
import FontAwesome


reader model =
    [ namedGrid Container
        { columns = [ px 300, fill 1, fill 0.2 ]
        , rows =
            [ px 1 => [ spanAll "separator" ]
            , px 40 => [ span 1 "TOCHeader", span 1 "contentHeader", span 1 "sideBarHeader" ]
            , px 650 => [ span 1 "TOC", span 1 "content", span 1 "sidebar" ]
            , px 40 => [ spanAll "footer" ]
            ]
        }
        []
        [ named "separator" (hairline Hairline)
        , named "TOCHeader"
            (toolSelectorPanel model)
        , named "contentHeader"
            (el TitleStyle [ paddingXY 10 8 ] (text model.current_document.title))
        , named "content"
            (text "")
        , named "TOC" (Common.tool model)
        , named "footer" (Component.footer model)
        ]
    ]


toolSelectorPanel model =
    row Panel
        [ paddingXY 10 6, spacing 15, center ]
        [ el Zero
            [ width (px 85)
            , onClick (SelectTool TableOfContents)
            , title "Table of contents"
            , height (px 30)
            , padding 2
            ]
            (html (FontAwesome.list (Component.toolSelectorColor model TableOfContents) 25))
        , el Zero
            [ width (px 85)
            , onClick (SelectTool ReaderTools)
            , title "Tools"
            , height (px 30)
            , padding 2
            ]
            (html (FontAwesome.gear (Component.toolSelectorColor model ReaderTools) 25))
        ]
