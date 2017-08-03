module Views.Home exposing (..)

import StyleSheet exposing (..)
import Element exposing (..)
import Element.Attributes exposing (..)
import Views.Signin as Signin
import Types exposing(Model, Msg)
import Views.Common as Common
import Views.Component as Component

home : Model -> List (Element Styles variation Msg)
home model =
    [ namedGrid Container
        { columns = [ fill 1, fill 2, fill 1 ]
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
               (Common.visibleIf model.appState.signedIn (text ("Signed in as " ++ model.current_user.username)))
              ,(Common.visibleIf model.appState.signedIn (Common.homepage model))
              ,(Common.getDocument "key=home" "Home pages" model)
              ,(Common.getDocument "ident=2017-8-2@16-32-16.2a43f7" "About Noteshare" model)
              ,(Common.getDocument "ident=2017-7-16@19-52-51.443e16" "User Manual" model)
              ,(Component.onlineStatusIndicator model)
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
                , (Common.visibleIf model.appState.signedIn (Common.documentListView "Recent" model))
                ]
            )
        ]
    ]
