module Views.Reader exposing (..)

import StyleSheet exposing (..)
import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events exposing (onClick)
import Views.Common as Common
import Views.Component as Component
import Types exposing (..)
import FontAwesome


reader : Model -> List (Element Styles variation Msg)
reader model =
    [ namedGrid Container
        { columns = [ px 300, fill 1, fill 0.2 ]
        , rows =
            [ px 1 => [ spanAll "separator" ]
            , px 40 => [ span 1 "TOCHeader", span 1 "contentHeader", span 1 "sideBarHeader" ]
            , fill 1 => [ span 1 "TOC", span 1 "content", span 1 "sidebar" ]

            ]
        }
        []
        [ named "separator" (hairline Hairline)
        , named "TOCHeader" (toolSelectorPanel model)
        , named "contentHeader" (contentHeader model)
        , named "content" (Common.renderedContent model)
        , named "TOC" (Common.tool model)

        ]
    ]


contentHeader : Model -> Element Styles variation Msg
contentHeader model =
  row NavBar [ justify ] [
    el TitleStyle [ paddingXY 10 8 ] (text model.current_document.title)
    , el AuthorStyle [ verticalCenter, paddingXY 10 8 ] (text model.current_document.authorName)
    ]


toolSelectorPanel : Model -> Element Styles variation Msg
toolSelectorPanel model =
    row Panel
        [ paddingXY 10 6, spacing 30 ]
        [ Common.printButton model.current_document
        , Common.selectTableOfContents model
        , Common.recallLastSearchButton model
        , selectReaderTools model
        ]


selectReaderTools : Model -> Element Styles variation Msg
selectReaderTools model =
    el Zero
        [ width (px 85)
        , onClick (SelectTool ReaderTools)
        , title "Tools"
        , height (px 30)
        , padding 2
        ]
        (html (FontAwesome.gear (Component.toolSelectorColor model ReaderTools) 25))
