port module OutsideInfo exposing (..)

import Json.Decode exposing (decodeValue)
import Json.Encode


sendInfoOutside : InfoForOutside -> Cmd msg
sendInfoOutside info =
    case info of
        PutTextToRender value ->
            infoForOutside { tag = "PutTextToRender", data = value }

 

-- getInfoFromOutside : (InfoForElm -> msg) -> (String -> msg) -> Sub msg
-- getInfoFromOutside tagger onError =
--     infoForElm
--         (\outsideInfo ->
--             case outsideInfo.tag of
--                 "EntriesChanged" ->
--                     case decodeValue (Json.Decode.list entryDecoder) outsideInfo.data of
--                         Ok entries ->
--                             tagger <| EntriesChanged entries

--                         Err e ->
--                             onError e

--                 _ ->
--                     onError <| "Unexpected info from outside: " ++ toString outsideInfo
--         )


type InfoForOutside
    = PutTextToRender Json.Encode.Value
    


type InfoForElm
    = EntriesChanged (List String)


type alias GenericOutsideData =
    { tag : String, data : Json.Encode.Value }


port infoForOutside : GenericOutsideData -> Cmd msg


port infoForElm : (GenericOutsideData -> msg) -> Sub msg