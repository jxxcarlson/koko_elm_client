port module OutsideInfo exposing (..)

import Json.Decode exposing (decodeValue, decodeString, string)
import Types exposing(
    Msg(..), 
    DocMsg(..), 
    InfoForOutside(..), 
    InfoForElm(..), 
    GenericOutsideData
    )
import Data.User


sendInfoOutside : InfoForOutside -> Cmd msg
sendInfoOutside info =
    case info of
        PutTextToRender value ->
            infoForOutside { tag = "PutTextToRender", data = value }
        WindowData value ->
            infoForOutside { tag = "WindowData", data = value }
        UserData value ->
            infoForOutside { tag = "UserData", data = value }
        UserState value ->
            infoForOutside { tag = "UserState", data = value }
        SaveDocumentStack value ->
            infoForOutside { tag = "SaveDocumentStack", data = value }
        AskToReconnectUser value ->
            infoForOutside { tag = "AskToReconnectUser", data = value }
        AskToRecoverUserState value ->
            infoForOutside { tag = "AskToRecoverUserState", data = value }
        DisconnectUser value ->
            infoForOutside { tag = "DisconnectUser", data = value }
 

getInfoFromOutside : (InfoForElm -> msg) -> (String -> msg) -> Sub msg
getInfoFromOutside tagger onError =
    infoForElm
        (\outsideInfo ->
            case outsideInfo.tag of 
                "RenderedText" -> 
                    case decodeValue string outsideInfo.data of
                        Ok renderedText ->
                            tagger <| RenderedText renderedText

                        Err e ->
                            onError e

                "RecoverUserState" ->
                    case decodeValue Data.User.userStateRecordDecoder outsideInfo.data of
                        Ok userStateRecord ->
                            tagger <| RecoveredUserState userStateRecord

                        Err e ->
                            onError e

                "ReconnectUser" ->
                    case decodeValue Data.User.localStorageUserDecoder outsideInfo.data of
                        Ok userLoginRecord ->
                            tagger <| UserLoginInfo userLoginRecord

                        Err e ->
                            onError e
                _   ->
                    onError <| "Unexpected info from outside: " ++ toString outsideInfo
        )


port infoForOutside : GenericOutsideData -> Cmd msg


port infoForElm : (GenericOutsideData -> msg) -> Sub msg

