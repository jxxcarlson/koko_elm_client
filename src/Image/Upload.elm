module Image.Upload exposing (..)

-- import Types exposing(Model, Image, defaultImage, Credentials, CredentialsResult)
-- https://elmseeds.thaterikperson.com/upload-to-s3

import Date exposing (Date)
import Date.Extra
import Http exposing (Request, stringPart)
import HttpBuilder as HB exposing (..)
import Image.FileReader as FR exposing (NativeFile)
import Json.Decode exposing (field)
import Request.Api
import Types exposing (..)


getUploadCredentials1 model =
    let
        image =
            model.imageRecord.mImage |> Maybe.withDefault defaultImage

        url =
            Request.Api.api ++ "/credentials?filename=" ++ image.filename ++ "&mimetype=image/jpeg&bucket=noteimages"

        cmd =
            Http.get url decodeCredentialsWrapper
                |> Http.send CredentialsResult
    in
    ( { model | message = "image: " ++ image.filename }, cmd )


getUploadCredentials : Model -> ( Model, Cmd Msg )
getUploadCredentials model =
    let
        filename =
            case model.fileToUpload of
                Just file ->
                    file.name

                Nothing ->
                    "xxx"

        _ =
            Debug.log "FILENAME = " filename

        mimeType_ =
            case model.fileToUpload of
                Just file ->
                    case file.mimeType of
                        Just mtype ->
                            toString mtype

                        Nothing ->
                            "xxx"

                Nothing ->
                    "xxx"

        mimeType =
            mimeType_ |> String.toLower |> String.split " " |> String.join "/"

        _ =
            Debug.log "MIMETYPE = " mimeType

        _ =
            Debug.log "FILE = " model.fileToUpload

        url =
            Request.Api.api ++ "/credentials?filename=" ++ filename ++ "&mimetype=" ++ mimeType ++ "&bucket=noteimages"

        cmd =
            HB.get url
                |> HB.withHeader "authorization" ("Bearer " ++ model.current_user.token)
                |> withExpect (Http.expectJson decodeCredentialsWrapper)
                |> HB.send (ImageMsg << CredentialsResult)
    in
    ( { model | message = "filename: " ++ filename }, cmd )


dateString : Date -> String
dateString date =
    let
        y =
            Date.year date |> toString

        m =
            Date.Extra.monthNumber date |> toString

        d =
            Date.day date |> toString

        md =
            List.map (String.padLeft 2 '0') [ m, d ] |> String.join ""
    in
    y ++ md



decodeCredentials : Json.Decode.Decoder Credentials
decodeCredentials =
    Json.Decode.map7 Credentials
        (field "x-amz-signature" Json.Decode.string)
        (field "x-amz-date" Json.Decode.string)
        (field "x-amz-credential" Json.Decode.string)
        (field "x-amz-algorithm" Json.Decode.string)
        (field "policy" Json.Decode.string)
        (field "key" Json.Decode.string)
        (field "acl" Json.Decode.string)
 
        
decodeCredentialsWrapper : Json.Decode.Decoder CredentialsWrapper
decodeCredentialsWrapper =
    Json.Decode.map2 CredentialsWrapper
        (field "credentials" decodeCredentials)
        (field "url" Json.Decode.string)


multiPartBody : Credentials -> FR.NativeFile -> Http.Body
multiPartBody creds nf =
    Http.multipartBody
        [ stringPart "key" nf.name
        , stringPart "x-amz-algorithm" creds.algorithm
        , stringPart "x-amz-credential" creds.credential
        , stringPart "x-amz-date" creds.date
        , stringPart "policy" creds.policy
        , stringPart "x-amz-signature" creds.signature
        , FR.filePart "file" nf
        ]


uploadRequest : Credentials -> NativeFile -> Request String
uploadRequest creds file =
    Http.request
        { method = "POST"
        , headers = []
        , url = "https://noteimages.s3.amazonaws.com"
        , body = multiPartBody creds file
        , expect = Http.expectString
        , timeout = Nothing
        , withCredentials = False
        }

request : Credentials -> Model -> (Model, Cmd Msg)
request credentials model =
    let
        _ = Debug.log "Image.upload.request credentials" credentials 
        cmd =
            model.fileToUpload
                |> Maybe.map
                    (\file ->
                        uploadRequest credentials file
                            |> Http.send (ImageMsg << UploadComplete)
                    )
                |> Maybe.withDefault Cmd.none
    in
    ( model, cmd )
