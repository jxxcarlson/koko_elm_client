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
    | NavBar
    | Button
    | ActiveButton
    | SearchField
    | Footer
    | Main
    | Page
    | Logo
    | NavOption
    | Box
    | Container
    | Label


{-| First, we create a stylesheet.
Styles only deal with properties that are not related to layout, position, or size.
Generally all properties only have one allowed unit, which is usually px.
If you want to use something like em
-}
stylesheet : StyleSheet Styles variation
stylesheet =
    Style.stylesheet
        [ style None [ Font.typeface [ "helvetica", "arial", "sans-serif" ] ]
          -- It's handy to have a blank style
        , style Main
            [ Border.all 1
              -- set all border widths to 1 px.
            , Color.text Color.darkCharcoal
            , Color.background Color.white
            , Color.border Color.lightGrey
            , Font.typeface [ "helvetica", "arial", "sans-serif" ]
            , Font.size 16
            , Font.lineHeight 1.3
              -- line height, given as a ratio of current font size.
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
            [ Transition.all
            , Color.text Color.white
            , Color.background Color.blue
            , Color.border Color.blue
            , Border.rounded 3
              -- round all borders to 3px
            , paddingHint 20
            , hover
                [ Color.text Color.white
                , Color.background Color.red
                , Color.border Color.red
                , cursor "pointer"
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
        , style Footer
            [ Color.background Color.charcoal
            , Color.text Color.white
            ]
        , style Button
            [ Color.text Color.white
            , Color.background Color.charcoal
            , Font.size 14
            , Border.rounded 6
            , Font.typeface [ "helvetica", "arial", "sans-serif" ]
            ]
        , style ActiveButton
            [ Color.text Color.white
            , Color.background Color.darkRed
            , Font.size 14
            , Border.rounded 6
            , Font.typeface [ "helvetica", "arial", "sans-serif" ]
            ]
        , style SearchField
            [ Color.text Color.black ]
        ]



-- el None [ center, width (px 800) ]


{-| Our view is made up of `Element`s,
which you can think of as Html with layout, positioning, and spacing built in.
-}
view _ =
    Element.root stylesheet <|
        column None
            []
            [ navigation
            , el None [ center, width (px 800) ] <|
                column Main
                    [ spacing 50, paddingTop 50, paddingBottom 50 ]
                    (List.concat
                        [ viewTextLayout
                        , viewRowLayouts
                        , viewGridLayout
                        , viewNamedGridLayout
                        ]
                    )
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



{-
   reader model =
       div []
           [ div [ id "toolSelectorPanel" ] [ toolSelectorPanel model ]
             -- div [ id "toolSelectorPanel" ] [ toolSelectorPanel model ]
           , div [ id "toolPane" ] [ toolSelector model ]
           , div [ id "titlePane" ] [ text model.current_document.title ]
             -- HERE use the node with id = rendered_text2 in JS-land.
           ]

-}
-- reader model =


{-| A text layout
-}
viewTextLayout2 =
    [ el Label [] (text "First, Some Text") ]


viewTextLayout =
    [ el Label [] (text "First, Some Text")
    , textLayout None
        [ spacingXY 25 25
        , padding 60
        ]
        [ el Box
            [ width (px 200)
            , height (px 300)
            , alignLeft
            ]
            (text "Alignment attributes (such as alignLeft), work in all layouts. In a text layout, the result is the element is floated left.")
        , paragraph None
            []
            [ text "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Mauris vel lectus eget lorem lobortis suscipit. Fusce porta auctor purus sed tempor. Mauris auctor sapien sit amet elementum egestas. Maecenas placerat consequat mauris, at dapibus enim tristique a. Quisque feugiat ultricies lorem nec volutpat. Sed risus enim, facilisis id fermentum quis, eleifend in diam. Suspendisse euismod, urna nec consectetur volutpat, massa libero aliquam urna, hendrerit venenatis leo lacus faucibus nulla. Curabitur et mattis dolor."
            ]
        , hairline Container
        , paragraph None
            []
            [ text "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Mauris vel lectus eget lorem lobortis suscipit. Fusce porta auctor purus sed tempor. Mauris auctor sapien sit amet elementum egestas. Maecenas placerat consequat mauris, at dapibus enim tristique a. Quisque feugiat ultricies lorem nec volutpat. Sed risus enim, facilisis id fermentum quis, eleifend in diam. Suspendisse euismod, urna nec consectetur volutpat, massa libero aliquam urna, hendrerit venenatis leo lacus faucibus nulla. Curabitur et mattis dolor."
            ]
        , paragraph None
            [ width (px 500)
            , center
            ]
            [ text "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Mauris vel lectus eget lorem lobortis suscipit. Fusce porta auctor purus sed tempor. Mauris auctor sapien sit amet elementum egestas. Maecenas placerat consequat mauris, at dapibus enim tristique a. Quisque feugiat ultricies lorem nec volutpat. Sed risus enim, facilisis id fermentum quis, eleifend in diam. Suspendisse euismod, urna nec consectetur volutpat, massa libero aliquam urna, hendrerit venenatis leo lacus faucibus nulla. Curabitur et mattis dolor."
            ]
        , paragraph None
            []
            [ text "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Mauris vel lectus eget lorem lobortis suscipit. Fusce porta auctor purus sed tempor. Mauris auctor sapien sit amet elementum egestas. Maecenas placerat consequat mauris, at dapibus enim tristique a. Quisque feugiat ultricies lorem nec volutpat. Sed risus enim, facilisis id fermentum quis, eleifend in diam. Suspendisse euismod, urna nec consectetur volutpat, massa libero aliquam urna, hendrerit venenatis leo lacus faucibus nulla. Curabitur et mattis dolor."
            ]
        , el Box
            [ width (px 200)
            , height (px 300)
            , alignRight
            , spacing 100
            ]
            empty
        , paragraph None
            []
            [ text "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Mauris vel lectus eget lorem lobortis suscipit. Fusce porta auctor purus sed tempor. Mauris auctor sapien sit amet elementum egestas. Maecenas placerat consequat mauris, at dapibus enim tristique a. Quisque feugiat ultricies lorem nec volutpat. Sed risus enim, facilisis id fermentum quis, eleifend in diam. Suspendisse euismod, urna nec consectetur volutpat, massa libero aliquam urna, hendrerit venenatis leo lacus faucibus nulla. Curabitur et mattis dolor."
            ]
        , paragraph None
            []
            [ text "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Mauris vel lectus eget lorem lobortis suscipit. Fusce porta auctor purus sed tempor. Mauris auctor sapien sit amet elementum egestas. Maecenas placerat consequat mauris, at dapibus enim tristique a. Quisque feugiat ultricies lorem nec volutpat. Sed risus enim, facilisis id fermentum quis, eleifend in diam. Suspendisse euismod, urna nec consectetur volutpat, massa libero aliquam urna, hendrerit venenatis leo lacus faucibus nulla. Curabitur et mattis dolor."
            ]
          -- a "full" will expand to consume the parent padding.
        , full Box [] <|
            text "A Full element will grow to consume parent padding!"
        ]
    ]


viewRowLayouts =
    [ el Label [] (text "Here is a Row Layout")
    , row Container
        [ spacingXY 20 20 ]
        [ el Box [ width (px 100), height (px 100) ] empty
        , el Box [ width (px 100), height (px 100) ] empty
        , el Box [ width (px 100), height (px 100) ] empty
        ]
    , el Label [] (text "You can Align Children Individually")
    , row Container
        [ spacingXY 20 20, height (px 400) ]
        [ el Box [ width (px 100), height (px 100), alignTop ] (text "top")
        , el Box [ width (px 100), height (px 100), verticalCenter ] (text "vcenter")
        , el Box [ width (px 100), height (px 100), alignBottom ] (text "bottom")
        ]
    , el Label [] (text "Or you can set the alignment for an entire layout.")
    , row Container
        [ spacingXY 20 20, alignRight ]
        [ el Box [ width (px 100), height (px 100) ] empty
        , el Box [ width (px 100), height (px 100) ] empty
        , el Box [ width (px 100), height (px 100) ] empty
        ]
    ]


viewGridLayout =
    [ el Label [] (text "Grid Layout")
    , grid Container
        { columns = [ px 100, px 100, px 100, px 100 ]
        , rows =
            [ px 100
            , px 100
            , px 100
            , px 100
            ]
        }
        [ spacing 20 ]
        [ area
            { start = ( 0, 0 )
            , width = 1
            , height = 1
            }
            (el Box [] (text "box"))
        , area
            { start = ( 1, 1 )
            , width = 1
            , height = 2
            }
            (el Box [ spacing 100 ] (text "box"))
        , area
            { start = ( 2, 1 )
            , width = 2
            , height = 2
            }
            (el Box [] (text "box"))
        , area
            { start = ( 1, 0 )
            , width = 1
            , height = 1
            }
            (el Box [] (text "box"))
        ]
    ]


viewNamedGridLayout =
    [ el Label [] (text "Named Grid Layout")
    , namedGrid Container
        { columns = [ px 200, px 200, px 200, fill 1 ]
        , rows =
            [ px 200 => [ spanAll "header" ]
            , px 200 => [ span 3 "content", span 1 "sidebar" ]
            , px 200 => [ span 3 "content", span 1 "sidebar" ]
            , px 200 => [ spanAll "footer" ]
            ]
        }
        []
        [ named "header"
            (el Box [] (text "box"))
        , named "sidebar"
            (el Box [] (text "box"))
        ]
    ]
