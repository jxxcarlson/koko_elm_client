module Views.Home exposing (..)

import StyleSheet exposing (..)
import Element exposing (..)
import Element.Attributes exposing (..)
import Views.Signin as Signin
import Types exposing(Model, Msg)
import Views.Common as Common
import Views.Component as Component
import Views.Utility as Utility

home : Model -> List (Element Styles variation Msg)
home model =
    [ namedGrid Container
        { columns = [ fill 1, fill 1, fill 2 ]
        , rows =
            [ -- px 40 => [ span 1 "TOCHeader", span 1 "contentHeader", span 1 "sideBarHeader" ]
              fill 2 => [ span 1 "LHSidebar", span 1 "Middle", span 1 "RHSidebar" ]
            ]
        }
        []
        [ named "LHSidebar"
           (column
             None
             [padding 40, spacing 20]
             [
               (Utility.visibleIf model.appState.signedIn (text ("Signed in as " ++ model.current_user.username)))
              ,(Utility.visibleIf model.appState.signedIn (Common.homepage model))
              ,(Common.getDocument BluishCharcoal "key=home" "Home pages" model)
              ,(Common.getDocument Blue "ident=2017-8-2@16-32-16.2a43f7" "About Noteshare" model)
              ,(Common.getDocument BluishCharcoal "ident=2017-7-16@19-52-51.443e16" "User Manual" model)
              -- ,(Component.onlineStatusIndicator model)
             ]
           )
          --     (Component.toolSelectorPanel model)
          -- , named "contentHeader"
          --     (el TitleStyle [ paddingXY 10 8 ] (text model.current_document.title))
          , named "Middle"
            (row
                None
                [ padding 40, spacing 20 ]
                [ (Signin.signinForm model)
                -- , (Signin.signoutForm model)
                , (Signin.registerUserForm model)
                , (Signin.signinInfoPanel model)
                , (Utility.visibleIf model.appState.signedIn (Common.documentListView "Recent" model))
                ]
            )

          , named "RHSidebar"
             (column
               None [spacing 15, paddingXY 20 40]
               [ specialTitle model
                , Common.specialContent model]

             )
        ]
    ]

specialTitle model =
  el TitleStyle [height (px 35),  verticalCenter, maxWidth (px 550), width (percent 100),
     paddingLeft 10] (el TitleStyle [ verticalCenter ] (text model.specialDocument.title))

specialTitle1 model =
  el TitleStyle [height (px 35),  verticalCenter, maxWidth (px 550), width (percent 100),
     paddingLeft 10] (text model.specialDocument.title)
