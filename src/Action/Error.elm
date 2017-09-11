module Action.Error exposing (httpErrorString)

import Http exposing (Error(..))
import List.Extra
import Json.Decode exposing (..)
import Dict


httpErrorString : Error -> String
httpErrorString error =
    case error of
        BadUrl text ->
            "Bad Url: " ++ text

        Timeout ->
            "Http Timeout"

        NetworkError ->
            "Network Error"

        BadStatus response ->
            "Bad Http Status: " ++ toString response.status.code

        BadPayload message response ->
            let
                errorDictResult =
                    message
                        |> String.split "got:"
                        |> List.Extra.last
                        |> Maybe.withDefault """{"error":"Unknown error"}"""
                        |> decodeString (dict string)

                errorMessage =
                    case errorDictResult of
                        Ok errorDict ->
                            errorDict |> Dict.values |> List.head |> Maybe.withDefault "Error"

                        Err err ->
                            "Error"
            in
                errorMessage
