module User.Auth exposing (..)

-- https://auth0.com/blog/creating-your-first-elm-app-part-2/

import Data.User exposing (jwtDecoder, registerUserEncoder, signinEncoder)
import External
import Http exposing (send)
import Json.Decode as Decode exposing (..)
import Jwt exposing (decodeToken)
import Request.Api exposing (loginUrl, registerUserUrl)
import Request.Document
import Types exposing (..)
import User.Request
import Utility exposing (gotoPage)
import Views.External


loginUserCmd : Model -> String -> Cmd Msg
loginUserCmd model loginUrl =
    Http.send GetTokenCompleted (loginUser model loginUrl)


registerUserCmd : Model -> String -> Cmd Msg
registerUserCmd model registerUserUrl =
    Http.send (Authentication << CompleteRegistration) (registerUser model registerUserUrl)



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


{-| One of the tasks of getTokenCompleted is to send user data to
local stoarge via ports so that it can be persisted between app reloads.
-}
getTokenCompleted : Model -> Result Http.Error String -> ( Model, Cmd Msg )
getTokenCompleted model result =
    case result of
        Ok newToken ->
            case Jwt.decodeToken jwtDecoder newToken of
                Ok value ->
                    let
                        user =
                            model.current_user

                        appState =
                            model.appState

                        updatedAppState =
                            { appState | page = StartPage, signedIn = True }

                        user2 =
                            Debug.log "user2"
                                { user
                                    | username = value.username
                                    , token = newToken
                                    , id = value.user_id
                                }
                    in
                    ( { model
                        | current_user = user2
                        , message = "Signed in as " ++ value.username
                        , warning = ""
                        , appState = updatedAppState -- appStateWithPage model StartPage
                      }
                    , Cmd.batch
                        [ User.Request.getUserState user2.id
                        , Utility.gotoPage model StartPage
                        , External.saveUserLogin (Views.External.userData user2.name user2.email user2.id user2.username newToken)
                        , Request.Document.getDocumentWithAuthenticatedQuery
                            (DocMsg << GetSpecialDocument)
                            user2.token
                            "key=sidebarNotes"
                        ]
                    )

                Err error ->
                    ( { model | warning = "Incorrect username or password (1)" }, Cmd.none )

        Err error ->
            let
                appState =
                    model.appState

                updatedAppState =
                    { appState | page = StartPage, signedIn = False }
            in
            ( { model
                | errorMsg = toString error
                , appState = updatedAppState
                , warning = "Incorrect username or password"
              }
            , Cmd.none
            )


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
