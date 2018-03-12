module Views.External exposing (..)

import Types exposing (Model, Page, Document)
import Json.Encode exposing (encode, object, int, string, bool, list)

encodeWindowData : Model -> Page -> Json.Encode.Value
encodeWindowData model page =
    let 
        signedIn =
            if model.current_user.token == "" then
                False
            else
                True
    in 
    Json.Encode.object
        [ ( "width", Json.Encode.int <| model.window.width )
        , ( "height", Json.Encode.int <| model.window.height )
        , ( "page", Json.Encode.string <| toString page )
        , ( "signed_in", Json.Encode.bool <| signedIn )
        ]

encodeUserData : String -> String -> Int -> String -> String -> Json.Encode.Value
encodeUserData name email userId username token =
    Json.Encode.object
                [ ( "name", Json.Encode.string name )
                , ( "email", Json.Encode.string email )
                , ( "id", Json.Encode.int userId )
                , ( "username", Json.Encode.string username )
                , ( "token", Json.Encode.string token )
                ]


windowSetup : Int -> Int -> Page -> Bool -> Bool -> String
windowSetup width height page online signed_in =
    let
        data =
            object
                [ ( "width", int width )
                , ( "height", int height )
                , ( "page", string (toString page) )
                , ( "online", bool online )
                , ( "signed_in", bool signed_in )
                ]

        json =
            encode 2 data
    in
        json


{-| This is the data sent via ports to saveUserLogin the user's
login information. See `External.saveUserLogin` in
User.Auth.getTokenCompleted
-}
userData : String -> String -> Int -> String -> String -> String
userData name email userId username token =
    let
        data =
            object
                [ ( "name", string name )
                , ( "email", string email )
                , ( "id", int userId )
                , ( "username", string username )
                , ( "token", string token )
                ]

        _ =
            Debug.log "userData" data
    in
        encode 2 data


documentData : Document -> String
documentData document =
    let
        data =
            object
                [ ( "content", string document.content )
                , ( "docType", string document.attributes.docType )
                ]
    in
        encode 2 data
