module StyleSheet exposing (..)

import Color
import Element exposing (..)
import Element.Attributes exposing (..)
import Style exposing (..)
import Style.Border as Border
import Style.Color as Color
import Style.Font as Font
import Style.Transition as Transition


lightBlueColor : Color.Color
lightBlueColor =
    Color.rgb 150 150 255


{-| A synonym for creating tuples. This will be included in the standard library soon.
1 => 2 == (1, 2)
-}
(=>) : a -> b -> ( a, b )
(=>) =
    (,)


{-| The type we use for identifiers for our styles.
-}
type Styles
    = None
    | ActiveButton
    | ActiveFlatButton
    | Blue
    | BluishCharcoal
    | Box
    | Button
    | ButtonReversed
    | Charcoal
    | ClearButton
    | Container
    | Field
    | FlatButton
    | FlatButtonBlue
    | FlatLinkBlue
    | Footer
    | FooterNote
    | Form
    | Hairline
    | HeaderLabel
    | Heading
    | HeadingAlternate
    | Label
    | Gray
    | LightGray
    | Logo
    | Main
    | MainContent
    | MainContentDark
    | Menu
    | Mono
    | NavBar
    | NavBarActive
    | NavBarActive2
    | NavOption
    | Page
    | PaleBlue
    | DocumentStackColor
    | PaleRed
    | PaleYellow
    | Panel
    | EditorPanel
    | PanelSmallType
    | PanelSmallTypeHeading
    | PanelInfo
    | PanelInfoRed
    | PanelInfoGreen
    | Radio
    | SearchField
    | Small
    | Smaller
    | StatusFailure
    | StatusSuccess
    | TOC
    | TOCItem
    | TOCItemLoading
    | TOCItemChild
    | TOCItemChildSelected
    | TOCItemMaster
    | TOCItemMasterSelected
    | TOCItemSelected
    | TOCTitle
    | TitleStyle
    | RHSidebar
    | RHSidebarHeader
    | AuthorStyle
    | WarningFooterNote
    | OKFooterNote
    | XXX
    | Zero
    | Transparent


{-| First, we create a stylesheet.
Styles only deal with properties that are not related to layout, position, or size.
Generally all properties only have one allowed unit, which is usually px.
If you want to use something like em
-}
stylesheet : StyleSheet Styles variation
stylesheet =
    Style.stylesheet
        [ style None
            [ Font.typeface [ "helvetica", "arial", "sans-serif" ]
            , Color.background Color.lightGray
            ]
        , style Zero [ Font.lineHeight 0 ]
        , style XXX [ Color.background (Color.rgb 200 200 255) ]
        , style Hairline [ Color.background Color.lightGray, Color.text Color.white ]
        , style MainContent [ Color.background Color.white, Font.typeface [ "STIX", "STIX-Web" ] ]
        , style MainContentDark [ Color.background Color.black, Color.text Color.white ]
        , style Main
            [-- Border.all 1
             --   -- set all border widths to 1 px.
             -- , Color.text Color.darkCharcoal
             -- , Color.background Color.white
             -- , Color.border Color.lightGrey
             -- , Font.typeface [ "helvetica", "arial", "sans-serif" ]
             -- , Font.size 16
             -- , Font.lineHeight 1.3
             --   -- line height, given as a ratio of current font size.
            ]
        , style Page
            [ Border.all 5
            , Border.solid
            , Color.text Color.darkCharcoal
            , Color.background Color.white
            , Color.border Color.lightGrey
            ]
        , style Label
            [ Font.size 18
            , Color.background Color.darkCharcoal
            , Color.text Color.white
            ]
        , style Logo
            [ Font.size 16
            , Color.text Color.white
            , Font.typeface [ "helvetica", "arial", "sans-serif" ]
            ]
        , style NavOption
            [ Font.size 16
            , Font.typeface [ "helvetica", "arial", "sans-serif" ]
            ]
        , style ClearButton
            [ Font.size 14
            , Color.background Color.gray
            ]
        , style Box
            [ Color.text Color.white
            , Color.background Color.blue
            , Color.border Color.white
            , Border.rounded 3

            -- round all borders to 3px
            , paddingHint 20
            , hover
                [ cursor "pointer"
                ]
            ]
        , style Container
            [ Color.text Color.black
            , Color.background Color.lightGrey
            , Color.border Color.lightGrey
            ]
        , style NavBar
            [ Color.background Color.lightCharcoal
            , Color.text Color.white
            ]
        , style NavBarActive
            [ Color.background Color.lightCharcoal
            , Color.text Color.white
            , pseudo "active" [ Transition.all, Color.background Color.darkBlue, Color.text Color.blue ]
            ]
        , style NavBarActive2
            [ Color.background Color.lightCharcoal
            , Color.text Color.blue
            , pseudo "active" [ Transition.all, Color.background Color.darkBlue, Color.text Color.blue ]
            ]
        , style Panel
            [ Color.background (Color.lightCharcoal) ]
        , style EditorPanel
            [ Color.background (Color.rgb 180 180 180) ]
        , style PanelSmallType
            [ Color.background (Color.rgb 180 180 180), Font.size 13]  
        , style PanelSmallTypeHeading
            [ Color.background (Color.rgb 180 180 180), Font.size 13, Font.weight 600]    
        , style PanelInfo
            [ Color.background Color.lightCharcoal, Color.text Color.lightGray, Font.lineHeight 1.3 ]
        , style PanelInfoRed
            [ Color.background Color.black, Color.text Color.red, Font.lineHeight 1.3 ]
        , style PanelInfoGreen
            [ Color.background Color.black, Color.text Color.green, Font.lineHeight 1.3 ]
        , style Radio
            [ Font.size 14, Color.background Color.lightGray ]
        , style TitleStyle
            [ Color.background (Color.rgb 210 210 210)
            , Color.text (Color.rgb 120 0 0)
            , Font.size 20
            , Font.lineHeight 1.3
            ]
        , style RHSidebar
            [ Color.background (Color.rgb 210 210 210)
            , Color.text (Color.rgb 120 0 0)
            , Font.size 14
            , Font.lineHeight 1.3
            , Border.solid
            , Color.border (Color.rgb 120 120 120)
            , Border.left 1.0
            ]
        , style RHSidebarHeader
            [ Color.background (Color.rgb 210 210 210)
            , Color.text (Color.rgb 120 0 0)
            , Font.size 18
            , Font.lineHeight 1.3
            , Border.solid
            , Color.border (Color.rgb 120 120 120)
            , Border.left 1.0
            , Border.bottom 1.0
            ]
        , style AuthorStyle
            [ Color.background (Color.rgb 210 210 210)
            , Color.text (Color.rgb 120 0 0)
            , Font.size 15
            , Font.lineHeight 1.1
            ]
        , style Heading
            [ Color.background Color.lightCharcoal
            , Color.text Color.white
            , Font.size 16
            , Font.lineHeight 1.3
            ]
        , style HeadingAlternate
            [ Color.background Color.blue
            , Color.text Color.white
            , Font.size 16
            , Font.lineHeight 1.3
            ]
        , style Footer
            [ Color.background Color.charcoal
            , Color.text Color.white
            ]
        , style Button
            [ Color.text Color.black
            , Color.background Color.gray
            , Font.size 14
            , Font.center
            , Border.rounded 6
            , Font.typeface [ "helvetica", "arial", "sans-serif" ]
            , pseudo "active" [ Transition.all, Color.background Color.lightBlue ]
            ]
        , style ButtonReversed
            [ Color.text Color.charcoal
            , Color.background Color.white
            , Font.size 14
            , Font.center
            , Border.rounded 6
            , Font.typeface [ "helvetica", "arial", "sans-serif" ]
            , pseudo "active" [ Transition.all, Color.background Color.lightCharcoal ]
            ]
        , style FlatButton
            [ Color.text Color.white
            , Color.background Color.charcoal
            , Font.size 14
            , Font.center
            , Font.typeface [ "helvetica", "arial", "sans-serif" ]
            , pseudo "active" [ Transition.all, Color.background Color.lightCharcoal ]
            ]
        , style FlatButtonBlue
            [ Color.text Color.white
            , Color.background Color.blue
            , Font.size 14
            , Font.center
            , Font.typeface [ "helvetica", "arial", "sans-serif" ]
            , pseudo "link" [ Transition.all, Color.background Color.darkBlue ]
            , pseudo "visited" [ Transition.all, Color.background Color.darkBlue ]
            , pseudo "hover" [ Transition.all, Color.background Color.darkRed ]
            , pseudo "active" [ Transition.all, Color.background Color.lightBlue ]
            ]
        , style FlatLinkBlue
            [ Color.text Color.blue
            , Font.size 16
            , Font.center
            , Font.typeface [ "helvetica", "arial", "sans-serif" ]
            , pseudo "link" [ Transition.all, Color.background Color.darkBlue ]
            , pseudo "visited" [ Transition.all, Color.background Color.darkBlue ]
            , pseudo "hover" [ Transition.all, Color.background Color.darkRed ]
            , pseudo "active" [ Transition.all, Color.background Color.lightBlue ]
            ]
        , style HeaderLabel
            [ Color.text Color.white
            , Color.background Color.lightCharcoal
            , Font.size 14
            , Font.center
            , Font.typeface [ "helvetica", "arial", "sans-serif" ]
            ]
        , style ActiveFlatButton
            [ Color.text Color.white
            , Color.background Color.darkRed
            , Font.size 14
            , Font.typeface [ "helvetica", "arial", "sans-serif" ]
            , pseudo "active" [ Transition.all, Color.background Color.lightBlue ]
            ]
        , style SearchField
            [ Color.text Color.black ]
        , style Form
            [ Color.background Color.darkGrey ]
        , style Field
            [ Color.text Color.black ]
        , style TOC
            [ Color.background Color.gray, Border.none ]
        , style TOCItem
            [ Color.text Color.blue ]
        , style TOCItemLoading
            [ Color.text Color.lightBlue ]
        , style TOCItemChild
            [ Color.text Color.blue ]
        , style TOCItemMaster
            [ Color.text (Color.rgb 125 0 0), Font.weight 600 ]
        , style TOCItemSelected
            [ Color.text Color.white, Color.background Color.blue ]
        , style TOCItemChildSelected
            [ Color.text Color.white, Color.background Color.blue ]
        , style TOCItemMasterSelected
            [ Color.text (Color.rgb 255 130 130), Color.background Color.blue, Font.weight 100 ]
        , style TOCTitle
            [ Font.size 13 ]
        , style Menu
            [ Color.background Color.charcoal ]
        , style FooterNote
            [ Color.background Color.charcoal
            , Color.text Color.white
            , pseudo "visited" [ Color.text Color.white ]
            , pseudo "hover" [ Color.text Color.yellow ]
            ]
        , style WarningFooterNote [ Color.background Color.red, Color.text Color.white ]
        , style OKFooterNote [ Color.background Color.darkGreen, Color.text Color.white ]
        , style StatusSuccess [ Color.background Color.darkGreen, Color.text Color.white ]
        , style StatusFailure [ Color.background Color.darkRed, Color.text Color.white ]
        , style Small [ Font.size 13 ]
        , style Smaller [ Font.size 10 ]
        , style Mono [ Font.typeface [ "Lucida Sans Unicode" ] ]
        , style PaleBlue [ Color.background (Color.rgb 200 200 255), Color.text Color.blue ]
        , style BluishCharcoal [ Color.background (Color.rgb 100 100 120), Color.text Color.white ]
        , style Blue [ Color.background Color.blue, Color.text Color.white ]
        , style DocumentStackColor [ Color.background (Color.rgb 196 201 206) ]
        , style PaleYellow [ Color.background (Color.rgb 255 255 200) ]
        , style PaleRed [ Color.background (Color.rgb 255 200 200) ]
        , style Transparent [ Color.background (Color.rgba 255 255 255 0.0) ]
        , style Gray [ Color.background Color.gray ]
        , style Charcoal
            [ Color.background Color.charcoal
            , Color.text Color.white
            , pseudo "active" [ Transition.all, Color.background Color.lightCharcoal, Color.text Color.blue ]
            ]
        , style LightGray [ Color.background Color.lightGray ]
        ]


navigation =
    row None
        [ justify, paddingXY 80 20 ]
        [ el Logo [] (text "Style Elements")
        , row None
            [ spacing 20 ]
            [ el NavOption [ alignBottom ] (text "share")
            , el NavOption [ alignBottom ] (text "about")
            , el NavOption [ alignBottom ] (text "user profile")
            ]
        ]
