module Utility exposing (..)

import Html exposing (..)
import Css exposing (asPairs)
import Json.Decode as Json exposing (int, list, string, float, Decoder)
import Html.Attributes exposing (..)
import Html.Events as HE exposing (on, keyCode)


--styles :  List Css.Mixin -> Html.Attribute msg


styles =
    Css.asPairs >> Html.Attributes.style


updateListAt : List a -> Int -> a -> List a
updateListAt list n newElement =
    List.take n list ++ newElement :: List.drop (n + 1) list


onKeyUp : (Int -> msg) -> Attribute msg
onKeyUp tagger =
    on "keyup" (Json.map tagger HE.keyCode)



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



-- signinButtonText : Model -> String
-- signinButtonText model =
--     if model.current_user.token == "" then
--         "Sign in"
--     else
--         "Sign out"
