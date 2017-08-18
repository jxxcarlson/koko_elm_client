module User.Auth exposing (..)

-- https://auth0.com/blog/creating-your-first-elm-app-part-2/

import Http exposing (send)
import Json.Decode as Decode exposing (..)
import Jwt exposing (decodeToken)
import Request.Api exposing (loginUrl, registerUserUrl)
import Data.User exposing (signinEncoder, jwtDecoder, registerUserEncoder)
import Types exposing (..)
import Utility exposing (gotoPage)
import External
import Views.External



loginUserCmd : Model -> String -> Cmd Msg
loginUserCmd model loginUrl =
    Http.send GetTokenCompleted (loginUser model loginUrl)


registerUserCmd : Model -> String -> Cmd Msg
registerUserCmd model registerUserUrl =
    Http.send CompleteRegistration (registerUser model registerUserUrl)



-- http://package.elm-lang.org/packages/elm-lang/http/1.0.0/Http#
-- http://package.elm-lang.org/packages/elm-lang/http/1.0.0/Http#send
-- http://stackoverflow.com/questions/12320467/jquery-cors-content-type-options
-- http://www.html5rocks.com/en/tutorials/cors/


loginUser : Model -> String -> Http.Request String
loginUser model loginUrl =
    let
        body =
            model
                |> signinEncoder
                |> Http.jsonBody
    in
        Http.post loginUrl body tokenDecoder


getTokenCompleted : Model -> Result Http.Error String -> ( Model, Cmd Msg )
getTokenCompleted model result =
    case (Debug.log "Result-getTokenCompleted" result) of
        Ok newToken ->
            case Jwt.decodeToken jwtDecoder newToken of
                Ok value ->
                    let
                        user =
                            model.current_user

                        appState = model.appState

                        updatedAppState  = {appState | page = HomePage, signedIn = Debug.log "signedIn" True}

                        updated_user =
                            { user | username = value.username, token = newToken }
                    in
                        ( { model
                            | current_user = updated_user
                            , message = "Signed in as " ++ value.username
                            , appState = updatedAppState -- appStateWithPage model HomePage
                          }
                        , Cmd.batch
                            [ Utility.gotoPage model HomePage
                            , External.persist (Views.External.userData user.name user.email value.username newToken False)
                            ]
                        )

                Err error ->
                    ( { model | message = "Incorrect username or password (1)" }, Cmd.none )

        Err error ->
          let
            appState = model.appState

            updatedAppState  = {appState | page = HomePage, signedIn = False}
          in
            ( { model
                | errorMsg = (toString error)
                , appState = updatedAppState
                , message = "Incorrect username or password"
              }
            , Cmd.none
            )



--               info = "Error .. " ++ (toString error)} , Cmd.none )
-- |> Debug.log "error in getTokenCompleted"


registerUser : Model -> String -> Http.Request UserRecord
registerUser model c =
    let
        body =
            model
                |> registerUserEncoder
                |> Http.jsonBody
    in
        Http.post registerUserUrl body Data.User.userRecordDecoder


tokenDecoder : Decoder String
tokenDecoder =
    Decode.field "token" Decode.string
