module Views2.Reader exposing (..)

import StyleSheet exposing (..)
import Element exposing (..)
import Element.Attributes exposing (..)
import Views2.Common as Common
import Views2.Component as Component


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
            (Component.toolSelectorPanel model)
        , named "contentHeader"
            (el TitleStyle [ paddingXY 10 8 ] (text model.current_document.title))
        , named "content"
            (text "")
        , named "TOC" (Common.tool model)
        , named "footer" (Component.footer model)
        ]
    ]
