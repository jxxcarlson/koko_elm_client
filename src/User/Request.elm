module User.Request exposing(getList, get, putCurrentUser)

import Types exposing(Model, User, Users, UsersRecord, Msg(GetUsers, GetUser, PutUser))
import Json.Encode
import Json.Decode exposing(field)
import Json.Decode.Pipeline

import HttpBuilder as HB exposing (..)
import Http exposing (send)

import Request.Api
import Data.User


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

-- get : Int -> Cmd Msg
get user_id =
    let
        url = Request.Api.api ++ "users/" ++ (toString user_id)
    in
        HB.get url
            -- |> HB.withHeader "Authorization" ("Bearer " ++ token)
            |> withExpect (Http.expectJson decodeUserRecord)
            |> HB.send GetUser


putCurrentUserRB : Model ->  RequestBuilder ()
putCurrentUserRB model =
    let
        params =
            encodeMinimalUserRecord model.current_user

        url = Request.Api.api ++ "users/" ++ (toString model.current_user.id)
    in
        HB.put url
            |> HB.withHeader "Authorization" ("Bearer " ++ model.current_user.token)
            |> withJsonBody params

putCurrentUser : Model -> Cmd Msg
putCurrentUser model =
    let
        request =
            putCurrentUserRB model
                |> HB.toRequest
    in
        Http.send PutUser request

decodeUserRecord : Json.Decode.Decoder Types.BigUserRecord
decodeUserRecord =
  Json.Decode.Pipeline.decode Types.BigUserRecord
      |> Json.Decode.Pipeline.required "user" decodeUser

decodeUsers : Json.Decode.Decoder UsersRecord
decodeUsers =
    Json.Decode.Pipeline.decode UsersRecord
        |> Json.Decode.Pipeline.required "users" (Json.Decode.list decodeUser)

decodeUser : Json.Decode.Decoder User
decodeUser =
    Json.Decode.Pipeline.decode User
        |> Json.Decode.Pipeline.required "name" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "id" (Json.Decode.int)
        |> Json.Decode.Pipeline.required "username" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "email" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "blurb" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "password" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "token" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "admin" (Json.Decode.bool)


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

encodeMinimalUserInfo : User -> Json.Encode.Value
encodeMinimalUserInfo user =
    Json.Encode.object
        [ ("blurb",  Json.Encode.string <| user.blurb)
        , ("id",  Json.Encode.int <| user.id)
        ]

encodeMinimalUserRecord : User -> Json.Encode.Value
encodeMinimalUserRecord user =
  Json.Encode.object
      [ ( "user"
        , encodeMinimalUserInfo user
        )
      ]

-- encodeUsers : (List User) -> Json.Encode.Value
-- encodeUsers record =
--     Json.Encode.object
--         [ ("users",  Json.Encode.list <| List.map User <| record.users)
--         ]
