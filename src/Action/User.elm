module Action.User exposing (..)

import Types exposing (..)
import External
import Views.External
import Data.User
import Request.Api
import Request.User

-- dummy comment

updateEmail : Model -> String -> ( Model, Cmd Msg )
updateEmail model email =
    let
        user =
            model.current_user

        updated_user =
            { user | email = email }
    in
        ( { model | current_user = updated_user }, Cmd.none )


updatePassword : Model -> String -> ( Model, Cmd Msg )
updatePassword model password =
    let
        user =
            model.current_user

        updated_user =
            { user | password = password }
    in
        ( { model | current_user = updated_user }, Cmd.none )


updateName : Model -> String -> ( Model, Cmd Msg )
updateName model name =
    let
        user =
            model.current_user

        updated_user =
            { user | name = name }
    in
        ( { model | current_user = updated_user }, Cmd.none )


updateUsername : Model -> String -> ( Model, Cmd Msg )
updateUsername model username =
    let
        user =
            model.current_user

        updated_user =
            { user | username = username }
    in
        ( { model | current_user = updated_user }, Cmd.none )


login : Model -> Model
login model =
    let
        appState =
            model.appState

        newAppState =
            { appState | page = Types.HomePage, signedIn = True, authorizing = False }

        searchState = model.searchState

        newSearchState = { searchState | domain = Private }

        user =
            model.current_user

        updatedUser =
            { user | password = "" }
    in
        { model |
           message = "LOGGING IN",
           appState = newAppState,
           searchState = newSearchState,
           current_user = updatedUser }

login2 : Model -> (Model, Cmd Msg)
login2 model =
  ( login model,  Request.User.loginUserCmd model Request.Api.loginUrl)
    --, Action.Document.search Private "sort=updated" HomePage model

signout : String -> Model -> ( Model, Cmd Msg )
signout message model =
    let
        user =
            model.current_user

        updated_user =
            {
              name = ""
            , username = ""
            , email = ""
            , password = ""
            , token = ""
            , admin = False
          }

        oldAppState =
            model.appState

        newAppState =
            { oldAppState | page = Types.HomePage, registerUser = False, signedIn = False, authorizing = False }

    in
        ( { model
            | current_user = updated_user
            , appState = newAppState
            , info = ""
            , message = message
          }
        , Cmd.batch[
           External.toJs (Views.External.windowData model HomePage)
           , External.disconnectUser "foo"
          ]
        )

doReconnectUser : String -> Model -> (Model, Cmd Msg)
doReconnectUser jsonString model =
  let
      _ = Debug.log "!!jsonString" jsonString
      maybeUserRecord =
          Data.User.userRecord jsonString
      _ = Debug.log "!!maybeUserRecord" maybeUserRecord      
  in
      case maybeUserRecord of
          Ok userRecord ->
              reconnectUser model userRecord

          Err error ->
              ( { model | info = "Sorry, I cannot reconnect you" }, Cmd.none )

reconnectUser : Model -> UserRecord -> ( Model, Cmd Msg )
reconnectUser model userRecord =
    let
        _ = Debug.log "reconnnectUser with userRecord" userRecord
        user =
            model.current_user

        current_user =
            { user
                | username = userRecord.username
                , token = userRecord.token
            }

        appState =
            model.appState

        newAppState =
            { appState | page = Types.HomePage, signedIn = True, authorizing = False }
    in
        ( { model
            | current_user = current_user
            , appState = newAppState
            , message = "User reconnected: " ++ userRecord.username
          }
        , Cmd.none
        )

shortUsername : Model -> String
shortUsername model =
  let
    shortName = model.current_user.username
        |> String.split("@")
        |> List.head
        |> Maybe.withDefault "---"
   in
    shortName
