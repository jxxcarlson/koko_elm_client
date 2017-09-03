module Views.Basic exposing (..)

import Color
import Element as EL exposing (..)
import Element.Attributes as EA exposing (..)
import StyleSheet exposing (..)
import Html

type alias AwesomeFont msg = Color.Color -> Int -> Html.Html msg


button
    : String
    -> a
    -> List (Element.Internal.Model.Attribute variation msg)
    -> Element a variation msg
button title style attrs  =
    el style ([ paddingLeft 8, height (px 30), width (px 200) ] ++ attrs) (label title style [])

label
    : String
    -> style
    -> List (Element.Internal.Model.Attribute variation msg)
    -> Element style variation msg
label text_ style attrs  =
    (el style ([ verticalCenter, height (px 25), paddingLeft 10] ++ attrs) (el style [verticalCenter] (text text_)))


faIcon
    : String
    -> (Color.Color -> number -> Html.Html msg)
    -> List (Element.Internal.Model.Attribute variation msg)
    -> Element Styles variation msg
faIcon title_ icon attrs =
  el NavBarActive
    ([height (px 25), width (px 25), alignBottom, title title_, verticalCenter ] ++ attrs)
    (iconWhite20 icon)

iconMaker: a -> b -> (a -> b -> Html.Html msg) -> Element style variation msg
iconMaker color iconSize icon =
    (EL.html (icon color iconSize))

iconWhite20
    : (Color.Color -> number -> Html.Html msg)
    -> Element style variation msg
iconWhite20 = iconMaker Color.white 25
