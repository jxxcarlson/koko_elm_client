module Action.User exposing(..)

import Types exposing(..)

updateEmail : Model -> String -> (Model, Cmd Msg)
updateEmail model email =
  let
    user = model.current_user
    updated_user = {user | email = email}
  in
    ( { model | current_user = updated_user } , Cmd.none )


updatePassword : Model -> String -> (Model, Cmd Msg)
updatePassword model password =
  let
    user = model.current_user
    updated_user = {user | password = password }
  in
    ( { model | current_user = updated_user } , Cmd.none )

updateName : Model -> String -> (Model, Cmd Msg)
updateName model name =
  let
    user = model.current_user
    updated_user = {user | name = name }
  in
    ( { model | current_user = updated_user } , Cmd.none )

updateUsername : Model -> String -> (Model, Cmd Msg)
updateUsername model username =
  let
    user = model.current_user
    updated_user = {user | username = username }
  in
    ( { model | current_user = updated_user } , Cmd.none )



signout : Model  -> (Model, Cmd Msg)
signout model =
  let
    user = model.current_user
    updated_user = User "" "" "" "" ""
  in
    ( { model | current_user = updated_user, registerUser = False, info = "" } , Cmd.none )
