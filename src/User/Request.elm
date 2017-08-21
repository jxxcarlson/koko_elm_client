module User.Request exposing(getList)

import Types exposing(User, Users, UsersRecord, Msg(GetUsers))
import Json.Encode
import Json.Decode exposing(field)
import Json.Decode.Pipeline

import HttpBuilder as HB exposing (..)
import Http exposing (send)

import Request.Api


getList : String -> Cmd Msg
getList query =
    let
        queries = if query == "" then
            [Request.Api.api ++ "users?public_user=yes"]
          else
            [Request.Api.api ++ "users?public_user=yes", query]
        url = queries |> String.join("&")
    in
        HB.get url
            -- |> HB.withHeader "Authorization" ("Bearer " ++ token)
            |> withExpect (Http.expectJson decodeUsers)
            |> HB.send GetUsers

decodeUser : Json.Decode.Decoder User
decodeUser =
    Json.Decode.map8 User
        (field "name" Json.Decode.string)
        (field "id" Json.Decode.int)
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
