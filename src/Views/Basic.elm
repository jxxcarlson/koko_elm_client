module Views.Basic exposing (..)


import Types exposing(Model, Msg)
-- import StyleSheet exposing (..)
import Color
import Element as EL exposing (..)
import Element.Attributes as EA exposing (..)
import Element.Events as EE exposing (..)
import FontAwesome
import StyleSheet exposing (..)
import Html

type alias AwesomeFont msg = Color.Color -> Int -> Html.Html msg


button title style attrs  =
    el style ([ paddingLeft 8, height (px 30), width (px 200) ] ++ attrs) (label title style)

label text style =
    (el style [ verticalCenter ] (EL.text text))

faIcon title_ icon attrs =
  el NavBar
    ([height (px 25), width (px 25), alignBottom, title title_, verticalCenter ] ++ attrs)
    (iconWhite20 icon)

iconMaker color iconSize icon =
    (EL.html (icon color iconSize))

iconWhite20 = iconMaker Color.white 25

     
