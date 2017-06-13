module Views.External exposing (..)

import Types exposing (Model, Page)
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
                , ( "online", bool model.online )
                , ( "signed_in", bool signedIn )
                ]
    in
        encode 2 data


windowSetup : Int -> Int -> Page -> Bool -> String
windowSetup width height page online =
    let
        data =
            object
                [ ( "width", int width )
                , ( "height", int height )
                , ( "page", string (toString page) )
                , ( "online", bool True )
                ]

        json =
            encode 2 data
    in
        json
