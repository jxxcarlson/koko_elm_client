module Utility.KeyValue
    exposing
        ( getIntValueForKeyFromTagList
        , getStringValueForKeyFromTagList
        , removeKeyInTagList
        , setIntValueForKeyInTagList
        )

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
                    keyValueIntHelper tag
    in
    value


getStringValueForKeyFromTagList : String -> List String -> Maybe String
getStringValueForKeyFromTagList key tags =
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
                    keyValueStringHelper tag
    in
    value


removeKeyInTagList : String -> List String -> List String
removeKeyInTagList key tags =
    tags |> List.filter (\tag -> not (String.startsWith (key ++ ":") tag))


setIntValueForKeyInTagList : String -> Int -> List String -> List String
setIntValueForKeyInTagList key value tags =
    tags |> removeKeyInTagList key |> (\list -> list ++ [ key ++ ":" ++ toString value ])


keyValueIntHelper : String -> Maybe Int
keyValueIntHelper tag =
    let
        maybeIdString =
            tag |> String.split ":" |> List.drop 1 |> List.head
    in
    case maybeIdString of
        Nothing ->
            Nothing

        Just idString ->
            idString |> String.toInt |> Result.toMaybe


keyValueStringHelper : String -> Maybe String
keyValueStringHelper tag =
    tag
        |> String.split ":"
        |> List.drop 1
        |> List.head
