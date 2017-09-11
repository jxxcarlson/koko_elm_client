module Views.Utility exposing (..)

import Element exposing (..)
import String.Extra
import StyleSheet exposing (..)
import Types exposing (Msg)


visibleIf : Bool -> Element Styles variation Msg -> Element Styles variation Msg
visibleIf condition body =
    if condition then
        body
    else
        el None [] (text "")


notVisibleIf : Bool -> Element Styles variation Msg -> Element Styles variation Msg
notVisibleIf condition body =
    if (not condition) then
        body
    else
        el None [] (text "")


shortString : Int -> String -> String
shortString nChars str =
    let
        parts =
            String.Extra.softBreak nChars str
    in
        if List.length parts > 1 then
            parts |> List.head |> Maybe.withDefault "" |> \str -> str ++ " ..."
        else
            str
