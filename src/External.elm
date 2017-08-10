port module External exposing (..)

import Json.Encode as Encode
import Types exposing (Document, ImagePortData)
import Document.Preprocess


{-| }
   render and getRenderedText establish a client-server
   relationship between the Elm app and JS-world
   where Asciidoctor.js and MathJax.js live.

   Use render (encodeDocument document) to send
   rendered_content to JS-world.
-}
port render : Encode.Value -> Cmd msg

{-|
   Subscribe to rendereed text.
-}
port getRenderedText : (String -> msg) -> Sub msg

{-|
encodeDocument is used to send rendered content to JS-world.
-}
encodeDocument : Document -> Encode.Value
encodeDocument document =
    [ ( "content", Encode.string (Document.Preprocess.preprocess document.content document) )
    , ( "textType", Encode.string document.attributes.textType )
    ]
        |> Encode.object


port toJs : String -> Cmd msg


port persist : String -> Cmd msg



-- Ask JS to send back the token, if any, in localStorage:


port askToReconnectUser : String -> Cmd msg


port reconnectUser : (String -> msg) -> Sub msg

port disconnectUser : String -> Cmd msg


-- TEST:


port toElm : (String -> msg) -> Sub msg

-- IMAGE UPLOAD

port fileSelected : String -> Cmd msg


port fileContentRead : (ImagePortData -> msg) -> Sub msg

-- @zghor: FILE UPLOAD

port fileUpload : String -> Cmd msg


port fileUploaded : (Bool -> msg) -> Sub msg
