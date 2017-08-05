module Views.Utility exposing(..)

import Element exposing (..)
import StyleSheet exposing (..)
import Types exposing(Msg)

visibleIf : Bool -> Element Styles variation Msg -> Element Styles variation Msg
visibleIf condition body =
    if condition then
        body
    else
        el None [] (text "")

notVisibleIf : Bool -> Element Styles variation Msg -> Element Styles variation Msg
notVisibleIf condition body =
    if (not condition)then
        body
    else
        el None [] (text "")
