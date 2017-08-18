module User.Request exposing(getList)

import Types exposing(User, Users, UsersRecord, Msg(GetUsers))
import Json.Encode
import Json.Decode exposing(field)
import Json.Decode.Pipeline

import HttpBuilder as HB exposing (..)
import Http exposing (send)

import Request.Api


getList :  Cmd Msg
getList =
    let
        _ = Debug.log "getList (xxx)" 1
        url = Request.Api.api ++ "/users"
    in
        HB.get url
            -- |> HB.withHeader "Authorization" ("Bearer " ++ token)
            |> withExpect (Http.expectJson decodeUsers)
            |> HB.send GetUsers

decodeUser : Json.Decode.Decoder User
decodeUser =
    Json.Decode.map7 User
        (field "name" Json.Decode.string)
        (field "username" Json.Decode.string)
        (field "email" Json.Decode.string)
        (field "blurb" Json.Decode.string)
        (field "password" Json.Decode.string)
        (field "token" Json.Decode.string)
        (field "admin" Json.Decode.bool)

encodeUser : User -> Json.Encode.Value
encodeUser user =
    Json.Encode.object
        [ ("name",  Json.Encode.string <| user.name)
        , ("username",  Json.Encode.string <| user.username)
        , ("email",  Json.Encode.string <| user.email)
        , ("password",  Json.Encode.string <| user.password)
        , ("token",  Json.Encode.string <| user.token)
        , ("admin",  Json.Encode.bool <| user.admin)
        ]


decodeUsers : Json.Decode.Decoder UsersRecord
decodeUsers =
    Json.Decode.Pipeline.decode UsersRecord
        |> Json.Decode.Pipeline.required "users" (Json.Decode.list decodeUser)

-- encodeUsers : (List User) -> Json.Encode.Value
-- encodeUsers record =
--     Json.Encode.object
--         [ ("users",  Json.Encode.list <| List.map User <| record.users)
--         ]
