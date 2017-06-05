module Request.User exposing(..)

-- https://auth0.com/blog/creating-your-first-elm-app-part-2/

import Http exposing (send)
import Json.Decode as Decode exposing (..)
import Jwt exposing(decodeToken)

import Request.Api exposing(loginUrl, registerUserUrl)
import Data.User exposing(signinEncoder, jwtDecoder, registerUserEncoder)
import Types exposing(..)


loginUserCmd : Model -> String -> Cmd Msg
loginUserCmd model loginUrl =
    Http.send GetTokenCompleted (loginUser model loginUrl)

registerUserCmd : Model -> String -> Cmd Msg
registerUserCmd model registerUserUrl =
    Http.send GetTokenCompleted (registerUser model registerUserUrl)


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
    case result of
        Ok newToken ->
           case Jwt.decodeToken jwtDecoder newToken of
             Ok value ->
              let
                user = model.current_user
                updated_user = { user | username = value.username, token = newToken }
              in
                ({model | current_user = updated_user,
                  info = "", -- "Success: signed in as " ++ value.username,
                  page = ReaderPage },
                Cmd.none)

             Err error ->({model | info = "Could not get authorization"}, Cmd.none)
        Err error ->
            ( { model | errorMsg = (toString error),
               info = "Bad username or password"} , Cmd.none )

--               info = "Error .. " ++ (toString error)} , Cmd.none )
-- |> Debug.log "error in getTokenCompleted"

registerUser : Model -> String -> Http.Request String
registerUser model c =
    let
        body =
            model
                |> registerUserEncoder
                |> Http.jsonBody
    in
        Http.post registerUserUrl body tokenDecoder


tokenDecoder : Decoder String
tokenDecoder =
    Decode.field "token" Decode.string
