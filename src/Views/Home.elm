module Views.Home exposing (..)

import Element exposing (..)
import Element.Attributes exposing (..)
import StyleSheet exposing (..)
import Types exposing (Model, Msg, Device(..))
import Views.Common as Common
import Views.Signin as Signin
import Views.TOC as TOC
import Views.Utility as Utility


home : Model -> List (Element Styles variation Msg)
home model =
    case model.device of
        Phone ->
            phoneHome model

        Tablet ->
            tabletHome model

        _ ->
            standardHome model


standardHome : Model -> List (Element Styles variation Msg)
standardHome model =
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
                [ padding 40, spacing 20 ]
                [ (Utility.visibleIf model.appState.signedIn (text ("Signed in as " ++ model.current_user.username)))
                , (Utility.notVisibleIf model.appState.signedIn (text "Not signed in"))

                -- ,(Basic.button 200 Blue "Home Pages" Types.GotoUserHomePages model)
                -- ,(Common.getDocument BluishCharcoal "ident=2017-8-10@4-50-53.515776" "Random Links" model)
                -- , el LightGray [height (px 20), width (px 200)] (text "")
                , (Common.getDocument Blue "ident=2017-8-2@16-32-16.2a43f7" "Examples" model)
                , (Common.getDocument BluishCharcoal "ident=2017-7-16@19-52-51.443e16" "Manual" model)
                , (Common.getDocument Blue "ident=2017-8-9@13-13-39.094b94" "About" model)
                ]
            )
        , named "Middle"
            (row
                None
                [ padding 40, spacing 20 ]
                [ Common.specialContent model
                ]
            )
        , named "RHSidebar"
            (column
                None
                [ spacing 15, paddingXY 20 40 ]
                []
            )
        ]
    ]


tabletHome : Model -> List (Element Styles variation Msg)
tabletHome model =
    [ namedGrid Container
        { columns = [ fill 1, fill 2 ]
        , rows =
            [ -- px 40 => [ span 1 "TOCHeader", span 1 "contentHeader", span 1 "sideBarHeader" ]
              fill 2 => [ span 1 "LHSidebar", span 1 "Middle" ]
            ]
        }
        []
        [ named "LHSidebar"
            (column
                None
                [ padding 40, spacing 20 ]
                [ (Utility.visibleIf model.appState.signedIn (text ("Signed in as " ++ model.current_user.username)))
                , (Utility.notVisibleIf model.appState.signedIn (text "Not signed in"))

                -- ,(Basic.button 200 Blue "Home Pages" Types.GotoUserHomePages model)
                , (Common.getDocument BluishCharcoal "ident=2017-8-10@4-50-53.515776" "Random Links" model)

                -- , el LightGray [height (px 20), width (px 200)] (text "")
                , (Common.getDocument Blue "ident=2017-8-2@16-32-16.2a43f7" "Examples" model)
                , (Common.getDocument BluishCharcoal "ident=2017-7-16@19-52-51.443e16" "Manual" model)
                , (Common.getDocument Blue "ident=2017-8-9@13-13-39.094b94" "System status and news" model)
                ]
            )
        , named "Middle"
            (row
                None
                [ padding 40, spacing 20 ]
                [ Common.specialContent model
                ]
            )
        ]
    ]


phoneHome : Model -> List (Element Styles variation Msg)
phoneHome model =
    phoneList model


phoneList : Model -> List (Element Styles variation Msg)
phoneList model =
    [ namedGrid Container
        { columns = [ fill 1, fill 2 ]
        , rows =
            [ -- px 40 => [ span 1 "TOCHeader", span 1 "contentHeader", span 1 "sideBarHeader" ]
              fill 2 => [ span 1 "Middle" ]
            ]
        }
        []
        [ named "Middle"
            (row
                Blue
                [ height (px 700), width (px 440) ]
                [ Common.specialContent model
                ]
            )
        ]
    ]


specialTitle : Model -> Element Styles variation msg
specialTitle model =
    el TitleStyle
        [ height (px 35)
        , verticalCenter
        , maxWidth (px 550)
        , width (percent 100)
        , paddingLeft 10
        ]
        (el TitleStyle [ verticalCenter ] (text model.specialDocument.title))


specialTitle1 : Model -> Element Styles variation msg
specialTitle1 model =
    el TitleStyle
        [ height (px 35)
        , verticalCenter
        , maxWidth (px 550)
        , width (percent 100)
        , paddingLeft 10
        ]
        (text model.specialDocument.title)
