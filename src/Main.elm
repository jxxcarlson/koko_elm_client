module Main exposing (..)

import Init
import Nav.Parser
import Navigation
import Subscriptions exposing (subscriptions)
import Types exposing (Flags, Model, Msg)
import Update.Main
import Views.Main


main : Program Flags Model Msg
main =
    Navigation.programWithFlags Nav.Parser.urlParser
        { init = Init.init
        , view = Views.Main.view
        , update = Update.Main.update
        , subscriptions = subscriptions
        }



-- ----
-- SendToJS str ->
--     ( model, toJs str )
-- LinkTo path ->
--     ( model, Navigation.newUrl path )
