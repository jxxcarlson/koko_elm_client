module Views2.Editor exposing (..)

import StyleSheet exposing (..)
import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events exposing (onInput)
import Views2.Common as Common
import Views2.Component as Component
import Types exposing (..)
import Element.Keyed as Keyed


editor model =
    [ namedGrid Container
        { columns = [ px 300, fill 1, fill 1 ]
        , rows =
            [ px 40 => [ span 1 "TOCHeader", span 1 "contentHeader", span 1 "editorPanel" ]
            , px 650 => [ span 1 "TOC", span 1 "content", span 1 "sidebar" ]
            , px 40 => [ spanAll "footer" ]
            ]
        }
        []
        [ named "TOCHeader"
            (Component.toolSelectorPanel model)
        , named "contentHeader"
            (inputText TitleStyle [ paddingXY 10 8, width (percent 100), height (percent 100), onInput Title, placeholder "Title" ] (model.current_document.title))
        , named "content"
            (Keyed.row None [] [ ( (toString model.counter), (textArea None [ width (percent 100), padding 8, onInput InputContent ] (model.current_document.content)) ) ])
        , named "TOC" (Common.tool model)
        , named "footer" (Component.footer model)
        , named "editorPanel" (Component.editorPanel model)
        ]
    ]
