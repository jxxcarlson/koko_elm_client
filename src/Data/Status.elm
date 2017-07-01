module Data.Status exposing (..)

import Types exposing (Status)
import Request.Api exposing (publicDocumentsUrl, documentsUrl)
import Json.Encode as Encode exposing (..)
import Json.Decode as Decode exposing (at, int, list, string, decodeString, Decoder)


decodeStatus : Decode.Decoder Status
decodeStatus =
    Decode.map Status
        (Decode.field "status" Decode.string)


encodeStatus : Status -> Encode.Value
encodeStatus record =
    Encode.object
        [ ( "status", Encode.string <| record.status )
        ]
