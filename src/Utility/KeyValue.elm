module Utility.KeyValue exposing (..)

{-| Tags the form k:v define a key-value pair.
The function extractValue key taglist resturns
the value of the key-value pair as a Maybe Int
-}

{- Key-value functions for a list of tags -}


getIntValueForKeyFromTagList : String -> List String -> Maybe Int
getIntValueForKeyFromTagList key tags =
    let
        maybeMacrotag =
            tags
                |> List.filter (\tag -> String.startsWith (key ++ ":") tag)
                |> List.head

        value =
            case maybeMacrotag of
                Nothing ->
                    Nothing

                Just tag ->
                    keyValueHelper tag
    in
    value


removeKeyInTagList : String -> List String -> List String
removeKeyInTagList key tags =
    tags |> List.filter (\tag -> not (String.startsWith (key ++ ":") tag))


setIntValueForKeyInTagList : String -> Int -> List String -> List String
setIntValueForKeyInTagList key value tags =
    tags |> removeKeyInTagList key |> (\list -> list ++ [ key ++ ":" ++ toString value ])


keyValueHelper : String -> Maybe Int
keyValueHelper tag =
    let
        maybeIdString =
            tag |> String.split ":" |> List.drop 1 |> List.head

        id =
            case maybeIdString of
                Nothing ->
                    Nothing

                Just idString ->
                    idString |> String.toInt |> Result.toMaybe
    in
    id
