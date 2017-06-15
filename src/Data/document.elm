module Data.Document exposing (..)

import Json.Encode as Encode exposing (..)
import Json.Decode as Decode exposing (at, int, list, string, decodeString, Decoder)
import Json.Decode.Pipeline as JPipeline exposing (decode, required, optional, hardcoded)
import Types exposing (..)


-- http://eeue56.github.io/json-to-elm/


type alias Documents =
    { documents : List Document }



-- Nested JSON: https://gist.github.com/hipertracker/36afd3fa89c1f446cddd0a1fd1d53b6b
-- Nested JSON: https://gist.github.com/hipertracker/6fcfcc340bc369740afa6b985e64e663
-- Also: https://github.com/eeue56/json-to-elm
--- And this: https://github.com/dragonwasrobot/json-schema-to-elm
-- ARCHITECURE: https://gist.github.com/jah2488/ca3310ad385957e2e616c646de2275fb
-- FLAGS: https://guide.elm-lang.org/interop/javascript.html#flags


documentEncoder : Document -> Encode.Value
documentEncoder document =
    Encode.object
        [ ( "document"
          , Encode.object
                [ ( "id", Encode.int document.id )
                , ( "author_id", Encode.int document.author_id )
                , ( "title", Encode.string document.title )
                , ( "content", Encode.string document.content )
                , ( "rendered_content", Encode.string document.rendered_content )
                ]
          )
        ]



-- DOCUMENT DECODERS


documentDecoder : Decoder Document
documentDecoder =
    decode Document
        |> JPipeline.required "id" Decode.int
        |> JPipeline.required "author_id" Decode.int
        |> JPipeline.required "title" Decode.string
        |> JPipeline.required "content" Decode.string
        |> JPipeline.required "rendered_content" Decode.string


documentRecordDecoder : Decoder DocumentRecord
documentRecordDecoder =
    JPipeline.decode DocumentRecord
        |> JPipeline.required "document" (documentDecoder)


document : String -> Result String Document
document jsonString =
    decodeString documentDecoder jsonString



-- DOCUMENTS DECODERS


decodeDocumentsRecord : Decoder Documents
decodeDocumentsRecord =
    JPipeline.decode Documents
        |> JPipeline.required "documents" (Decode.list documentDecoder)


documentsDecoder : Decoder Documents
documentsDecoder =
    decode
        Documents
        |> required "documents" (Decode.list documentDecoder)


documents : String -> Result String Documents
documents jsonString =
    decodeString documentsDecoder jsonString
