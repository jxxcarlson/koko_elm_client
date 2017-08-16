module Image.Upload exposing(..)

-- import Types exposing(Model, Image, defaultImage, Credentials, CredentialsResult)
import Types exposing(..)
import Json.Decode exposing(field)
import Date.Extra
import Date exposing(Date)

import Http exposing(stringPart, Request)
import HttpBuilder as HB exposing (..)
import Image.FileReader as FR exposing(NativeFile)

getUploadCredentials1 model =
  let
    image = model.imageRecord.mImage |> Maybe.withDefault defaultImage
    url = "http://localhost:4000/api/credentials?filename=" ++ image.filename ++ "&mimetype=image/jpeg&bucket=noteimages"
    cmd = Http.get url decodeCredentialsWrapper
      |> Http.send CredentialsResult
  in
     ( { model | message = "image: " ++ image.filename }, cmd)

getUploadCredentials : Model -> (Model, Cmd Msg)
getUploadCredentials model =
    let
        filename = case model.fileToUpload of
            Just file -> file.name
            Nothing -> "xxx"

        _ = Debug.log "FILENAME = " filename

        mimeType_ = case model.fileToUpload of
            Just file ->
              case file.mimeType of
                Just mtype ->
                  toString mtype
                Nothing ->
                  "xxx"
            Nothing ->
                "xxx"

        mimeType = mimeType_ |> String.toLower |> String.split " " |> String.join "/"

        _ = Debug.log "MIMETYPE = " mimeType

        _ = Debug.log "FILE = " model.fileToUpload

        url = "http://localhost:4000/api/credentials?filename=" ++ filename ++ "&mimetype=" ++ mimeType ++ "&bucket=noteimages"

        cmd = HB.get url
            |> HB.withHeader "authorization" ("Bearer " ++ model.current_user.token)
            |> withExpect (Http.expectJson decodeCredentialsWrapper)
            |> HB.send CredentialsResult

    in
       ( { model | message = "filename: " ++ filename }, cmd)

dateString : Date -> String
dateString date =
  let
    y = Date.year date |> toString
    m = Date.Extra.monthNumber date |> toString
    d = Date.day date |> toString
    md = List.map (String.padLeft 2 '0') [m,d] |> String.join ""
  in
    y ++ md

getFormattedDate : Maybe Date -> String
getFormattedDate date =
    case date of
        Just d ->
            dateString d

        Nothing ->
            "19010101"


awzCredential : Credentials -> String
awzCredential credentials  =
  let
    accessKeyId = credentials.awsAccessKeyId
    date = credentials.date
    cred = accessKeyId ++ "/" ++ date ++ "/us-east-1/s3/aws4_request"
    _ = Debug.log "cred:" cred
  in
    cred

decodeCredentials : Json.Decode.Decoder Credentials
decodeCredentials =
    Json.Decode.map6 Credentials
        (field "signature" Json.Decode.string)
        (field "policy" Json.Decode.string)
        (field "key" Json.Decode.string)
        (field "acl" Json.Decode.string)
        (field "AWSAccessKeyId" Json.Decode.string)
        (field "date" Json.Decode.string)

decodeCredentialsWrapper : Json.Decode.Decoder CredentialsWrapper
decodeCredentialsWrapper =
    Json.Decode.map2 CredentialsWrapper
        (field "credentials" decodeCredentials)
        (field "url" Json.Decode.string)


multiPartBody : Credentials -> FR.NativeFile -> Http.Body
multiPartBody creds nf =
  let
    _ = Debug.log "key" nf.name
    _ = Debug.log "x-amz-date" creds.date
    _ = Debug.log "x-amz-credential" (awzCredential creds)
  in
    Http.multipartBody
        [ stringPart "key" (nf.name)
        , stringPart "x-amz-algorithm" "AWS4-HMAC-SHA256"
        , stringPart "x-amz-credential" (awzCredential creds)
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

request result model =
  let
      _ = Debug.log "credentials" result
      _ = Debug.log "awzCredential = " (awzCredential result)
      _ = Debug.log "model.fileToUpload" model.fileToUpload
      cmd =
          model.fileToUpload
              |> Maybe.map
                  (\file ->
                      uploadRequest result file
                          |> Http.send UploadComplete
                  )
              |> Maybe.withDefault Cmd.none
  in
      (model , cmd )
