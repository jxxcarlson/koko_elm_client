module Data.User
    exposing
        ( signinEncoder
        , jwtDecoder
        , registerUserEncoder
        , userRecord
        , userRecordDecoder
        , localStorageUserRecord
        )

import Json.Encode as Encode exposing (..)
import Json.Decode exposing (at, int, list, string, decodeString, Decoder)
import Json.Decode.Pipeline as JPipeline exposing (decode, required, optional, hardcoded)
import Types exposing (Model, LoginUserRecord, UserRecord, ErrorMessage, LoginLocalStorageRecord)


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
                [ ( "name", Encode.string <| model.current_user.name )
                , ( "id", Encode.int <| model.current_user.id )
                , ( "username", Encode.string <| model.current_user.username )
                , ( "email", Encode.string <| model.current_user.email )
                , ( "password", Encode.string <| model.current_user.password )
                , ( "token", Encode.string <| model.current_user.token )
                , ( "admin", Encode.bool <| model.current_user.admin )
                , ( "blurb", Encode.string <| model.current_user.blurb )
                ]
          )
        ]



-- Encode.object
--     [ ( "user"
--       , Encode.object
--             [ ( "name", Encode.string model.current_user.name )
--             , ( "username", Encode.string model.current_user.username )
--             , ( "email", Encode.string model.current_user.email )
--             , ( "password", Encode.string model.current_user.password )
--             , ( "admin"), Encode.bool model.current_user.admin
--             ]
--       )
--     ]


type alias Claims =
    { username : String, user_id : Int }


jwtDecoder : Decoder Claims
jwtDecoder =
    decode Claims
        |> JPipeline.required "username" Json.Decode.string
        |> JPipeline.required "user_id" Json.Decode.int


userRecordDecoder : Decoder UserRecord
userRecordDecoder =
    JPipeline.decode UserRecord
        |> JPipeline.required "user" (userDecoder)


userDecoder : Decoder LoginUserRecord
userDecoder =
    decode LoginUserRecord
        |> JPipeline.required "name" Json.Decode.string
        |> JPipeline.required "username" Json.Decode.string
        |> JPipeline.required "id" Json.Decode.int
        |> JPipeline.required "email" Json.Decode.string
        |> JPipeline.required "token" Json.Decode.string
        |> JPipeline.required "blurb" Json.Decode.string


localStorageUserDecoder : Decoder LoginLocalStorageRecord
localStorageUserDecoder =
    decode LoginLocalStorageRecord
        |> JPipeline.required "name" Json.Decode.string
        |> JPipeline.required "username" Json.Decode.string
        |> JPipeline.required "id" Json.Decode.string
        |> JPipeline.required "email" Json.Decode.string
        |> JPipeline.required "token" Json.Decode.string
        |> JPipeline.required "blurb" Json.Decode.string



-- |> JPipeline.required "admin" Json.Decode.string |> Json.Decode.andThen fixup


fixup : String -> Bool
fixup str =
    case str of
        "false" ->
            False

        "true" ->
            True

        _ ->
            False


userRecord : String -> Result String LoginUserRecord
userRecord jsonString =
    decodeString userDecoder jsonString


localStorageUserRecord : String -> Result String LoginLocalStorageRecord
localStorageUserRecord jsonString =
    decodeString localStorageUserDecoder jsonString
