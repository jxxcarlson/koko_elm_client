module Utility exposing (..)

import Html.Attributes exposing (..)
import Css exposing (asPairs)


styles =
    Css.asPairs >> Html.Attributes.style
