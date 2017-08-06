module Data.User exposing (signinEncoder, jwtDecoder, registerUserEncoder, userRecord, userRecordDecoder)

import Json.Encode as Encode exposing (..)
import Json.Decode exposing (at, int, list, string, decodeString, Decoder)
import Json.Decode.Pipeline as JPipeline exposing (decode, required, optional, hardcoded)
import Types exposing (Model, UserRecord, ErrorMessage)


signinEncoder : Model -> Encode.Value
signinEncoder model =
    Encode.object
        [ ( "authenticate"
          , Encode.object
                [ ( "email", Encode.string model.current_user.email )
                , ( "password", Encode.string model.current_user.password )
                ]
          )
        ]


registerUserEncoder : Model -> Encode.Value
registerUserEncoder model =
    Encode.object
        [ ( "user"
          , Encode.object
                [ ( "name", Encode.string model.current_user.name )
                , ( "username", Encode.string model.current_user.username )
                , ( "email", Encode.string model.current_user.email )
                , ( "password", Encode.string model.current_user.password )
                ]
          )
        ]


type alias Claims =
    { username : String, user_id : Int }


jwtDecoder : Decoder Claims
jwtDecoder =
    decode Claims
        |> JPipeline.required "username" Json.Decode.string
        |> JPipeline.required "user_id" Json.Decode.int


userRecordDecoder : Decoder UserRecord
userRecordDecoder =
    decode UserRecord
        |> JPipeline.required "name" Json.Decode.string
        |> JPipeline.required "username" Json.Decode.string
        |> JPipeline.required "email" Json.Decode.string
        |> JPipeline.required "token" Json.Decode.string


userRecord : String -> Result String UserRecord
userRecord jsonString =
    decodeString userRecordDecoder jsonString
