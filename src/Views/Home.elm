module Views.Home exposing (..)

import StyleSheet exposing (..)
import Element exposing (..)
import Element.Attributes exposing (..)
import Views.Common as Common
import Views.Component as Component
import Views.Signin as Signin


home model =
    [ namedGrid Container
        { columns = [ fill 1, fill 2, fill 1 ]
        , rows =
            [ -- px 40 => [ span 1 "TOCHeader", span 1 "contentHeader", span 1 "sideBarHeader" ]
              px 650 => [ span 1 "LHSidebar", span 1 "Middle", span 1 "RHSidebar" ]
            ]
        }
        []
        [ -- named "TOCHeader"
          --     (Component.toolSelectorPanel model)
          -- , named "contentHeader"
          --     (el TitleStyle [ paddingXY 10 8 ] (text model.current_document.title))
          named "Middle"
            (row
                None
                [ padding 40, spacing 20 ]
                [ (Signin.signinForm model)
                , (Signin.signoutForm model)
                , (Signin.registerUserForm model)
                , el Box [ width (px 400), height (px 200) ] (text model.message)
                ]
            )
        ]
    ]
