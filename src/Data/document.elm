module Data.Document exposing (..)

import Json.Encode as Encode exposing (..)
import Json.Decode as Decode exposing (at, int, list, string, decodeString, Decoder)
import Json.Decode.Pipeline as JPipeline exposing (decode, required, optional, hardcoded)
import Types exposing (..)


-- http://eeue56.github.io/json-to-elm/
-- https://github.com/eeue56/json-to-elm
-- https://medium.com/@eeue56/how-i-implemented-json-to-elm-b61081587c3a


decodeChild : Decode.Decoder Child
decodeChild =
    Decode.map5 Child
        (Decode.field "title" Decode.string)
        (Decode.field "level" Decode.int)
        (Decode.field "doc_identifier" Decode.string)
        (Decode.field "doc_id" Decode.int)
        (Decode.field "comment" Decode.string)


encodeChild : Child -> Encode.Value
encodeChild record =
    Encode.object
        [ ( "title", Encode.string <| record.title )
        , ( "level", Encode.int <| record.level )
        , ( "doc_identifier", Encode.string <| record.doc_identifier )
        , ( "doc_id", Encode.int <| record.doc_id )
        , ( "comment", Encode.string <| record.comment )
        ]


type alias Documents =
    { documents : List Document }



-- Nested JSON: https://gist.github.com/hipertracker/36afd3fa89c1f446cddd0a1fd1d53b6b
-- Nested JSON: https://gist.github.com/hipertracker/6fcfcc340bc369740afa6b985e64e663
-- Also: https://github.com/eeue56/json-to-elm
--- And this: https://github.com/dragonwasrobot/json-schema-to-elm
-- ARCHITECURE: https://gist.github.com/jah2488/ca3310ad385957e2e616c646de2275fb
-- FLAGS: https://guide.elm-lang.org/interop/javascript.html#flags


documentEncoder0 : Document -> Encode.Value
documentEncoder0 document =
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


documentEncoder : Document -> Encode.Value
documentEncoder document =
    Encode.object
        [ ( "document"
          , documentEncoder1 document
          )
        ]


encodeDocumentAttributes : DocumentAttributes -> Encode.Value
encodeDocumentAttributes record =
    Encode.object
        [ ( "text_type", Encode.string <| record.textType )
        , ( "public", Encode.bool <| record.public )
        , ( "doc_type", Encode.string <| record.docType )
        ]


documentEncoder1 : Document -> Encode.Value
documentEncoder1 document =
    Encode.object
        [ ( "title", Encode.string <| document.title )
        , ( "rendered_content", Encode.string <| document.rendered_content )
        , ( "id", Encode.int <| document.id )
        , ( "identifier", Encode.string <| document.identifier )
        , ( "content", Encode.string <| document.content )
        , ( "author_id", Encode.int <| document.author_id )
        , ( "attributes", encodeDocumentAttributes <| document.attributes )
        , ( "tags", Encode.list <| List.map Encode.string <| document.tags )
        ]



-- DOCUMENT DECODERS


documentDecoder : Decoder Document
documentDecoder =
    decode Document
        |> JPipeline.required "id" Decode.int
        |> JPipeline.required "identifier" Decode.string
        |> JPipeline.required "author_id" Decode.int
        |> JPipeline.required "title" Decode.string
        |> JPipeline.required "content" Decode.string
        |> JPipeline.required "rendered_content" Decode.string
        |> JPipeline.required "attributes" (decodeDocumentAttributes)
        |> JPipeline.required "tags" (Decode.list Decode.string)


documentRecordDecoder : Decoder DocumentRecord
documentRecordDecoder =
    JPipeline.decode DocumentRecord
        |> JPipeline.required "document" (documentDecoder)


document : String -> Result String Document
document jsonString =
    decodeString documentDecoder jsonString


decodeDocumentAttributes : Decoder DocumentAttributes
decodeDocumentAttributes =
    JPipeline.decode DocumentAttributes
        |> JPipeline.required "public" (Decode.bool)
        |> JPipeline.required "text_type" (Decode.string)
        |> JPipeline.required "doc_type" (Decode.string)



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
