module Data.User
    exposing
        ( signinEncoder
        , jwtDecoder
        , registerUserEncoder
        , userRecord
        , userRecordDecoder
        , decodeUserStateRecord
        , encodeUserState
        , localStorageUserRecord
        )

import Json.Encode as Encode
import Json.Decode exposing (map, map2, map3, field, at, int, list, string, decodeString, Decoder)
import Json.Decode.Pipeline as JPipeline exposing (decode, required, optional, hardcoded)
import Types exposing (Model, LoginUserRecord, UserRecord, ErrorMessage, LoginLocalStorageRecord, UserStateRecord)


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


string2IntList str =
    str
        |> String.split ","
        |> List.map String.toInt
        |> List.map (Result.withDefault -1)
        |> List.filter (\x -> x /= -1)


userStateRecordDecoder : Decoder UserStateRecord
userStateRecordDecoder =
    map2 UserStateRecord (map string2IntList (field "documentStack" string)) (map String.toInt (field "currentDocumentId" string))


decodeUserStateRecord : String -> Result String UserStateRecord
decodeUserStateRecord jsonString =
    decodeString userStateRecordDecoder jsonString


encodeUserState : Model -> String
encodeUserState model =
    let
        ids =
            List.map (\doc -> doc.id) model.documentStack |> encodeIntegerList

        currentDocumentId =
            Encode.int model.current_document.id

        data =
            (Encode.object [ ( "documentStack", ids ), ( "currentDocumentId", currentDocumentId ) ])
    in
        Encode.encode 2 data


encodeIntegerList : List Int -> Encode.Value
encodeIntegerList ints =
    ints |> List.map Encode.int |> Encode.list


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
