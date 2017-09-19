module Views.Admin exposing (..)

import Types exposing (Model)
import User.Display
import StyleSheet exposing (..)
import Element exposing (..)
import Element.Attributes exposing (..)
import Types exposing (..)


admin : Model -> List (Element Styles variation Msg)
admin model =
    [ namedGrid Container
        [ padding 20 ]
        { columns = [ px 300, fill, percent 15 ]
        , rows =
            [ px 1 => [ spanAll "separator" ]
            , px 40 => [ span 1 "TOCHeader", span 1 "contentHeader", span 1 "sideBarHeader" ]
            , fill => [ span 1 "TOC", span 1 "content", span 1 "sidebar" ]
            ]
        , cells =
            [ named "TOC"
                (column None
                    []
                    [ (User.Display.list "Users" model)
                    ]
                )
            ]
        }
    ]
