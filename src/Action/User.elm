module Action.User exposing (..)

import Types exposing (Model)
import User.Request


updateCurrentUser model =
    let
        currentUser =
            model.current_user

        blurb_ =
            if model.textInputBuffer /= "" then
                model.textInputBuffer
            else
                model.current_user.blurb

        updatedCurrentUser =
            { currentUser | blurb = blurb_ }

        newModel =
            { model | current_user = updatedCurrentUser }
    in
        ( newModel, User.Request.putCurrentUser newModel )
