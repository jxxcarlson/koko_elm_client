port module External exposing (..)

import Document.Preprocess
import Json.Encode as Encode
import String.Extra
import Types exposing (Document, ImagePortData)




{-| encodeDocument is used to send rendered content to JS-world.
force = False \ True, with True as the default.
-}
encodeDocument : Bool -> List String -> Bool -> Document -> Encode.Value
encodeDocument force idList textNeedsUpdate document =
    let
        _ =
            Debug.log "::Encode document " document.id

        textType =
            document.attributes.textType

        content_to_render =
            case ( textType, textNeedsUpdate ) of
                ( "latex", True ) ->
                    document.rendered_content

                ( "latex", False ) ->
                    document.rendered_content

                ( _, _ ) ->
                    Document.Preprocess.preprocess document.content document

        idValueList =
            List.map Encode.string idList

        _ =
            Debug.log "(encodeDocument) port:: send to JSw, id:" document.id
    in
    [ ( "force", Encode.bool force )
    , ( "idList", Encode.list idValueList )
    , ( "id", Encode.int document.id )
    , ( "content", Encode.string content_to_render )
    , ( "textType", Encode.string document.attributes.textType )
    ]
        |> Encode.object


compress : String -> String
compress str =
    str |> String.Extra.replace " " "" |> String.Extra.replace "\n" ""



{- OUTGOING -}

port disconnectUser : String -> Cmd msg

-- IMAGE UPLOAD

port fileSelected : String -> Cmd msg

port fileUpload : String -> Cmd msg



{- INCOMING -}

port fileContentRead : (ImagePortData -> msg) -> Sub msg

port fileUploaded : (Bool -> msg) -> Sub msg

-- @zghor: FILE UPLOAD




