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
            case outsideInfo.tag of 
                "RenderedText" -> 
                    case decodeValue string outsideInfo.data of
                        Ok renderedText ->
                            tagger <| RenderedText (Debug.log "ptx, (ok, 1)" renderedText)

                        Err e ->
                            onError (Debug.log "ptx (err,2)" e)

                _   ->
                    onError <| "Unexpected info from outside: " ++ toString outsideInfo
        )


port infoForOutside : GenericOutsideData -> Cmd msg


port infoForElm : (GenericOutsideData -> msg) -> Sub msg