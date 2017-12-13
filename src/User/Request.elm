module User.Request exposing (encodeUserState, get, getList, getUserState, putCurrentUser, putUserState, putUserStateRecord)

import Data.User exposing (userStateRecordDecoder2)
import Http exposing (send)
import HttpBuilder as HB exposing (..)
import Json.Decode exposing (field)
import Json.Decode.Pipeline
import Json.Encode
import Request.Api
import Types exposing (Model, Msg(GetUser, GetUserState, GetUsers, PutUser), User, UserStateRecord, Users, UsersRecord)


getList : String -> Cmd Msg
getList query =
    let
        _ =
            Debug.log "User.getList, query" query

        queries =
            if query == "" then
                [ Request.Api.api ++ "users?public_user=yes" ]
            else
                [ Request.Api.api ++ "users?public_user=yes", query ]

        url =
            queries |> String.join "&"
    in
    HB.get url
        -- |> HB.withHeader "Authorization" ("Bearer " ++ token)
        |> withExpect (Http.expectJson decodeUsers)
        |> HB.send GetUsers


get : Int -> Cmd Msg
get user_id =
    let
        url =
            Request.Api.api ++ "users/" ++ toString user_id
    in
    HB.get url
        -- |> HB.withHeader "Authorization" ("Bearer " ++ token)
        |> withExpect (Http.expectJson decodeUserRecord)
        |> HB.send GetUser


getUserState : Int -> Cmd Msg
getUserState userId =
    let
        _ =
            Debug.log "getUserState" "CALLED"

        url =
            Request.Api.api ++ "users/getuserstate/" ++ toString userId
    in
    HB.get url
        -- |> HB.withHeader "Authorization" ("Bearer " ++ model.current_user.token)
        |> withExpect (Http.expectJson userStateRecordDecoder2)
        |> HB.send GetUserState


putCurrentUserRB : Model -> RequestBuilder ()
putCurrentUserRB model =
    let
        params =
            encodeMinimalUserRecord model.current_user

        url =
            Request.Api.api ++ "users/" ++ toString model.current_user.id
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


putUserState : Model -> Cmd Msg
putUserState model =
    let
        _ =
            Debug.log "putUserState" "NOW"

        params =
            encodeUserState model

        url =
            Request.Api.api ++ "users/saveuserstate/" ++ toString model.current_user.id

        request =
            HB.put url
                |> HB.withHeader "Authorization" ("Bearer " ++ model.current_user.token)
                |> withJsonBody params
                |> HB.toRequest
    in
    Http.send PutUser request


putUserStateRecord : UserStateRecord -> Model -> Cmd Msg
putUserStateRecord userStateRecord model =
    let
        params =
            encodeUserStateRecord userStateRecord

        url =
            Request.Api.api ++ "users/saveuserstate/" ++ toString model.current_user.id

        request =
            HB.put url
                |> HB.withHeader "Authorization" ("Bearer " ++ model.current_user.token)
                |> withJsonBody params
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
        |> Json.Decode.Pipeline.required "name" Json.Decode.string
        |> Json.Decode.Pipeline.required "id" Json.Decode.int
        |> Json.Decode.Pipeline.required "username" Json.Decode.string
        |> Json.Decode.Pipeline.required "email" Json.Decode.string
        |> Json.Decode.Pipeline.required "blurb" Json.Decode.string
        |> Json.Decode.Pipeline.required "password" Json.Decode.string
        |> Json.Decode.Pipeline.required "token" Json.Decode.string
        |> Json.Decode.Pipeline.required "admin" Json.Decode.bool


encodeUser : User -> Json.Encode.Value
encodeUser user =
    Json.Encode.object
        [ ( "name", Json.Encode.string <| user.name )
        , ( "username", Json.Encode.string <| user.username )
        , ( "email", Json.Encode.string <| user.email )
        , ( "password", Json.Encode.string <| user.password )
        , ( "token", Json.Encode.string <| user.token )
        , ( "admin", Json.Encode.bool <| user.admin )
        ]


encodeMinimalUserInfo : User -> Json.Encode.Value
encodeMinimalUserInfo user =
    Json.Encode.object
        [ ( "blurb", Json.Encode.string <| user.blurb )
        , ( "id", Json.Encode.int <| user.id )
        ]


encodeMinimalUserRecord : User -> Json.Encode.Value
encodeMinimalUserRecord user =
    Json.Encode.object
        [ ( "user"
          , encodeMinimalUserInfo user
          )
        ]


string2IntList str =
    str
        |> String.split ","
        |> List.map String.toInt
        |> List.map (Result.withDefault -1)
        |> List.filter (\x -> x /= -1)


encodeIntegerList : List Int -> Json.Encode.Value
encodeIntegerList ints =
    ints |> List.map Json.Encode.int |> Json.Encode.list


encodeUserStateRecord : UserStateRecord -> Json.Encode.Value
encodeUserStateRecord userStateRecord =
    let
        ids =
            userStateRecord.documentIntStack |> encodeIntegerList

        currentDocumentId =
            case userStateRecord.currentDocumentId of
                Ok id ->
                    Json.Encode.int id

                Err _ ->
                    Json.Encode.int 0

        -- data =
        --     (Json.Encode.object [ ( "current_document_id", currentDocumentId ), ( "id_list", ids ) ])
    in
    Json.Encode.object [ ( "current_document_id", currentDocumentId ), ( "id_list", ids ) ]


encodeUserState : Model -> Json.Encode.Value
encodeUserState model =
    let
        ids =
            List.map (\doc -> doc.id) model.documentStack |> encodeIntegerList

        currentDocumentId =
            Json.Encode.int model.current_document.id

        -- data =
        --     (Json.Encode.object [ ( "current_document_id", currentDocumentId ), ( "id_list", ids ) ])
    in
    Json.Encode.object [ ( "current_document_id", currentDocumentId ), ( "id_list", ids ) ]



-- Json.Encode.encode 2 data
-- encodeUsers : (List User) -> Json.Encode.Value
-- encodeUsers record =
--     Json.Encode.object
--         [ ("users",  Json.Encode.list <| List.map User <| record.users)
--         ]
