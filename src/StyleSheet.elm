module StyleSheet exposing (..)

import Color
import Element exposing (..)
import Element.Attributes exposing (..)
import Html
import Style exposing (..)
import Style.Border as Border
import Style.Color as Color
import Style.Font as Font
import Style.Transition as Transition


{-| A synonym for creating tuples. This will be included in the standard library soon.
1 => 2 == (1, 2)
-}
(=>) =
    (,)


{-| The type we use for identifiers for our styles.
-}
type Styles
    = None
    | Zero
    | XXX
    | Hairline
    | Panel
    | PanelInfo
    | NavBar
    | Menu
    | TitleStyle
    | Heading
    | Button
    | ActiveButton
    | FlatButton
    | ActiveFlatButton
    | SearchField
    | Field
    | Form
    | TOC
    | TOCItem
    | TOCItemSelected
    | Radio
    | Footer
    | FooterNote
    | Main
    | Page
    | Logo
    | NavOption
    | Box
    | Container
    | Label
    | StatusSuccess
    | StatusFailure


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
        , style XXX [ Color.background Color.lightBlue ]
        , style Hairline [ Color.background Color.lightGray, Color.text Color.white ]
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
            [ Font.size 25
              -- set font size to 25 px
            , Font.center
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
        , style Box
            [ Color.text Color.white
            , Color.background Color.charcoal
            , Color.border Color.gray
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
        , style Panel
            [ Color.background Color.lightCharcoal ]
        , style PanelInfo
            [ Color.background Color.lightCharcoal, Color.text Color.lightGray, Font.lineHeight 1.3 ]
        , style Radio
            [ Font.size 14, Color.background Color.lightBlue ]
        , style TitleStyle
            [ Color.background Color.lightCharcoal
            , Color.text Color.white
            , Font.size 20
            , Font.lineHeight 1.3
            ]
        , style Heading
            [ Color.background Color.lightCharcoal
            , Color.text Color.white
            , Font.size 16
            , Font.lineHeight 1.3
            ]
        , style Footer
            [ Color.background Color.charcoal
            , Color.text Color.white
            ]
        , style Button
            [ Color.text Color.white
            , Color.background Color.charcoal
            , Font.size 14
            , Font.center
            , Border.rounded 6
            , Font.typeface [ "helvetica", "arial", "sans-serif" ]
            ]
        , style FlatButton
            [ Color.text Color.white
            , Color.background Color.charcoal
            , Font.size 14
            , Font.center
            , Font.typeface [ "helvetica", "arial", "sans-serif" ]
            ]
        , style ActiveFlatButton
            [ Color.text Color.white
            , Color.background Color.darkRed
            , Font.size 14
            , Font.typeface [ "helvetica", "arial", "sans-serif" ]
            ]
        , style SearchField
            [ Color.text Color.black ]
        , style Form
            [ Color.background Color.darkGrey ]
        , style Field
            [ Color.text Color.black ]
        , style TOC
            [ Color.background Color.gray ]
        , style TOCItem
            [ Color.text Color.black ]
        , style TOCItemSelected
            [ Color.text Color.white, Color.background Color.charcoal ]
        , style Menu
            [ Color.background Color.charcoal ]
        , style FooterNote [ Color.background Color.charcoal, Color.text Color.white ]
        , style StatusSuccess [ Color.background Color.darkGreen, Color.text Color.white ]
        , style StatusFailure [ Color.background Color.darkRed, Color.text Color.white ]
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
