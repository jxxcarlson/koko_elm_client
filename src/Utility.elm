module Utility exposing (..)

import Html.Attributes exposing (..)
import Css exposing (asPairs)


styles =
    Css.asPairs >> Html.Attributes.style



-- signinButtonText : Model -> String
-- signinButtonText model =
--     if model.current_user.token == "" then
--         "Sign in"
--     else
--         "Sign out"
