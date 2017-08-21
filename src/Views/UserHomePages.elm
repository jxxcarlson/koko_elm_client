module Views.UserHomePages exposing (..)

import Types exposing(Model)

import User.Display

import StyleSheet exposing (..)
import Element exposing (..)
import Element.Attributes exposing (..)
import Views.Common as Common
import Types exposing (..)
import Element.Keyed as Keyed
import User.Search




userHomePages : Model -> List (Element Styles variation Msg)
userHomePages model =
    [ namedGrid Container
        { columns = [ px 300, fill 1, fill 0.2 ]
        , rows =
            [ px 1 => [ spanAll "separator" ]
            , px 40 => [ span 1 "TOCHeader", span 1 "contentHeader", span 1 "sideBarHeader" ]
            , fill 1 => [ span 1 "TOC", span 1 "content", span 1 "sidebar" ]

            ]
       }
       [padding 20]
       [ named "TOC"
         (column None [] [
             (User.Search.form model)
             , (User.Display.list "Home Pages" model)
            ]
        )
        , named "content" (Keyed.column
            Zero
            []
            [("Foo", Common.renderedContent model)]
          )

      ]


    ]
