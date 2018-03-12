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


port toJs : String -> Cmd msg


port saveUserLogin : String -> Cmd msg


port saveUserState : String -> Cmd msg


port saveDocumentStack : String -> Cmd msg


port saveCurrentDocumentId : String -> Cmd msg


port askToRecoverUserState : String -> Cmd msg


port recoverUserState : (String -> msg) -> Sub msg



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
