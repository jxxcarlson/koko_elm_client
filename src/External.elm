port module External exposing (..)

import Json.Encode as Encode
import Types exposing(Document)


port render : Encode.Value -> Cmd msg

-- type alias Value = {
--   content: String,
--   docType: String
-- }

-- encodeDocument { content, docType } =
--   [ ("content", Encode.string content), ("docType", Encode.string docType) ] |> Encode.object

encodeDocument : Document -> Encode.Value
encodeDocument document =
  [ ("content", Encode.string document.content), ("docType", Encode.string document.attributes.docType) ]
  |> Encode.object


port toJs : String -> Cmd msg


port persist : String -> Cmd msg



-- Ask JS to send back the token, if any, in localStorage:


port askToReconnectUser : String -> Cmd msg


port reconnectUser : (String -> msg) -> Sub msg


-- TEST:


port toElm : (String -> msg) -> Sub msg
