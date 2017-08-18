module Views.UserHomePages exposing (..)

import Types exposing(Model)

import User.Display

import StyleSheet exposing (..)
import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events exposing (onClick)
import Views.Common as Common
import Views.Component as Component
import Types exposing (..)
import FontAwesome



userHomePages : Model -> List (Element Styles variation Msg)
userHomePages model =
    [ namedGrid Container
        { columns = [ px 300, fill 1, fill 0.2 ]
        , rows =
            [ px 1 => [ spanAll "separator" ]
            -- , px 40 => [ span 1 "TOCHeader", span 1 "contentHeader", span 1 "sideBarHeader" ]
            , fill 1 => [ span 1 "TOC", span 1 "content", span 1 "sidebar" ]

            ]
       }
       [padding 20]
       [ named "TOC"
         (column None [] [
             (User.Display.list "Users" model)
            ]
        )
        , named "content" (Common.renderedContent model)
      ]


    ]
