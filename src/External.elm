port module External exposing (..)

import Json.Encode as Encode
import Types exposing (Document, ImagePortData)


{-| }
   Use render (encodeDocument document) to send
   rendered_content to JS-world.
-}
port render : Encode.Value -> Cmd msg


{-|
encodeDocument is used to send rendered content to JS-world.
-}
encodeDocument : Document -> Encode.Value
encodeDocument document =
    [ ( "content", Encode.string (document.content) )
    , ( "textType", Encode.string document.attributes.textType )
    ]
        |> Encode.object


port toJs : String -> Cmd msg


port persist : String -> Cmd msg



-- Ask JS to send back the token, if any, in localStorage:


port askToReconnectUser : String -> Cmd msg


port reconnectUser : (String -> msg) -> Sub msg

port getRenderedText : (String -> msg) -> Sub msg

-- TEST:


port toElm : (String -> msg) -> Sub msg

-- IMAGE UPLOAD

port fileSelected : String -> Cmd msg


port fileContentRead : (ImagePortData -> msg) -> Sub msg

-- @zghor: FILE UPLOAD

port fileUpload : String -> Cmd msg


port fileUploaded : (Bool -> msg) -> Sub msg
