module Views.External exposing (..)

import Types exposing (Model, Page, Document)
import Json.Encode exposing (encode, object, int, string, bool)


windowData : Model -> Page -> String
windowData model page =
    let
        signedIn =
            if model.current_user.token == "" then
                False
            else
                True

        data =
            object
                [ ( "width", int model.window.width )
                , ( "height", int model.window.height )
                , ( "page", string (toString page) )
                , ( "signed_in", bool signedIn )
                ]
    in
        encode 2 data


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


userData : String -> String -> String -> String -> Bool -> String
userData name email username token admin =
    let
        data =
            object
                [ ( "name", string name )
                , ( "email", string email )
                , ( "username", string username )
                , ( "token", string token )
                , ( "admin", bool admin)
                ]
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
