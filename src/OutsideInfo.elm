port module OutsideInfo exposing (..)

import Json.Decode exposing (decodeValue, decodeString, string)
import Json.Encode
import Types exposing(Msg(..), DocMsg(..), InfoForOutside(..), InfoForElm(..), GenericOutsideData)


sendInfoOutside : InfoForOutside -> Cmd msg
sendInfoOutside info =
    case info of
        PutTextToRender value ->
            infoForOutside { tag = "PutTextToRender", data = value }

 

getInfoFromOutside : (InfoForElm -> msg) -> (String -> msg) -> Sub msg
getInfoFromOutside tagger onError =
    infoForElm
        (\outsideInfo ->
            -- case decodeValue (Json.Decode.list entryDecoder) outsideInfo.data of
            case decodeString string outsideInfo.data of
                        Ok renderedText ->
                            tagger <| (EntriesChanged) entries

                        Err e ->
                            onError e


                _ ->
                    onError <| "Unexpected info from outside: " ++ toString outsideInfo
        )


port infoForOutside : GenericOutsideData -> Cmd msg


port infoForElm : (GenericOutsideData -> msg) -> Sub msg