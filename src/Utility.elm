module Utility exposing (..)

import Element
import Element.Events as EE exposing (keyCode, on)
import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Decode as Json exposing (Decoder, float, int, list, string)
import Regex
import String.Extra
import Types exposing (Model, Page, InfoForOutside(WindowData))
import Views.External
import OutsideInfo 


queryText : String -> String
queryText text =
    if text == "" then
        "all"
    else
        text


updateListAt : List a -> Int -> a -> List a
updateListAt list n newElement =
    List.take n list ++ newElement :: List.drop (n + 1) list


onKeyUp : (Int -> msg) -> Element.Attribute variation msg
onKeyUp tagger =
    on "keyup" (Json.map tagger keyCode)


gotoPage : Model -> Page -> Cmd msg
gotoPage model p =
    OutsideInfo.sendInfoOutside (WindowData <| Views.External.encodeWindowData model p)
    


-- youtube : String -> Html


youtube : String -> Html msg
youtube url =
    div [ class "embed-responsive embed-responsive-16by9" ]
        [ iframe
            [ class "embed-responsive-item"
            , src url
            ]
            []
        ]



-- onKeyUp2 (SomeMessage model.someInfo)
-- from List.Extra:


replaceIf : (a -> Bool) -> a -> List a -> List a
replaceIf predicate replacement list =
    List.map
        (\item ->
            if predicate item then
                replacement
            else
                item
        )
        list


removeWhen : (a -> Bool) -> List a -> List a
removeWhen pred list =
    List.filter (not << pred) list


{-| map str to lower case and squeeze out bad characters
-}
compress : String -> String -> String
compress replaceBlank str =
    str
        |> String.toLower
        |> String.Extra.replace " " replaceBlank
        |> Regex.replace Regex.All (Regex.regex "[,;.!?&_]") (\_ -> "")


addLineNumbers text =
    text
        |> String.trim
        |> String.split "\n"
        |> List.foldl addNumberedLine ( 0, [] )
        |> Tuple.second
        |> List.reverse
        |> String.join "\n"


addNumberedLine line data =
    let
        ( k, lines ) =
            data
    in
    ( k + 1, [ numberedLine (k + 1) line ] ++ lines )


numberedLine k line =
    String.padLeft 5 ' ' (toString k) ++ "  " ++ line



-- signinButtonText : Model -> String
-- signinButtonText model =
--     if model.current_user.token == "" then
--         "Sign in"
--     else
--         "Sign out"
