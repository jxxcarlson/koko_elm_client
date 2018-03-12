module Data.User
    exposing
        ( signinEncoder
        , jwtDecoder
        , registerUserEncoder
        , userRecord
        , userRecordDecoder
        , userStateRecordDecoder
        , userStateRecordDecoder2
        , decodeUserStateRecord
        , encodeDocumentStack
        , encodeUserState
        , encodeUserStateAsValue
        , localStorageUserRecord
        , localStorageUserDecoder
        )

import Json.Encode as Encode
import Json.Decode exposing (decodeValue, map, map2, map3, field, at, int, list, string, decodeString, Decoder)
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
    map3 UserStateRecord
        (field "documentStack" (list int))
        (map Ok (field "currentDocumentId" int))
        (field "token" string)


userStateRecordDecoder2 : Decoder UserStateRecord
userStateRecordDecoder2 =
    map3 UserStateRecord
        (field "documentStack" (list int))
        (map Ok (field "currentDocumentId" int))
        (field "token" string)


decodeUserStateRecord : Encode.Value -> Result String UserStateRecord
decodeUserStateRecord jsonValue =
    decodeValue userStateRecordDecoder jsonValue




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

encodeUserStateAsValue : Model -> Encode.Value
encodeUserStateAsValue model =
    let
        ids =
            List.map (\doc -> doc.id) model.documentStack 

        currentDocumentId =
            Encode.int model.current_document.id
   
    in
        Encode.object  [ 
            ( "documentStack", Encode.list (List.map Encode.int ids) )
          , ( "currentDocumentId", Encode.int model.current_document.id ) 
        ]

encodeDocumentStack : Model -> Encode.Value
encodeDocumentStack model =
    let
        ids =
            List.map (\doc -> doc.id) model.documentStack 
    in 
        Encode.object [ ( "documentStack", Encode.list (List.map Encode.int ids) ) ]
  

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
