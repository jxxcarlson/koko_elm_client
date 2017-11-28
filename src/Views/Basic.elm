module Views.Basic exposing (..)

import Color
import Element as EL exposing (..)
import Element.Attributes as EA exposing (..)
import StyleSheet exposing (..)
import Html


type alias AwesomeFont msg =
    Color.Color -> Int -> Html.Html msg


button title style attrs =
    el style ([ paddingLeft 8, height (px 30), width (px 200) ] ++ attrs) (label title style [])


label text_ style attrs =
    (el style ([ verticalCenter, height (px 25) ] ++ attrs) (el style [ verticalCenter ] (text text_)))


faIcon title_ icon attrs =
    el NavBarActive
        ([ height (px 25), width (px 20), alignBottom, title title_, verticalCenter ] ++ attrs)
        (iconWhite20 icon)


faIcon2 title_ icon attrs =
    el NavBarActive2
        ([ height (px 25), width (px 20), alignBottom, title title_, verticalCenter ] ++ attrs)
        (iconGreen20 icon)


iconMaker : a -> b -> (a -> b -> Html.Html msg) -> Element style variation msg
iconMaker color iconSize icon =
    (EL.html (icon color iconSize))


iconWhite20 :
    (Color.Color -> number -> Html.Html msg)
    -> Element style variation msg
iconWhite20 =
    iconMaker Color.white 25


iconGreen20 :
    (Color.Color -> number -> Html.Html msg)
    -> Element style variation msg
iconGreen20 =
    iconMaker Color.green 25
