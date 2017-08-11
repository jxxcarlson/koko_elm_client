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
        image = model.imageRecord.mImage |> Maybe.withDefault defaultImage
        url = "http://localhost:4000/api/credentials?filename=" ++ image.filename ++ "&mimetype=image/jpeg&bucket=noteimages"

        cmd = HB.get url
            |> HB.withHeader "authorization" ("Bearer " ++ model.current_user.token)
            |> withExpect (Http.expectJson decodeCredentialsWrapper)
            |> HB.send CredentialsResult

    in
       ( { model | message = "image: " ++ image.filename }, cmd)

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


awzCredential : Model -> Credentials -> String
awzCredential model credentials  =
  let
    accessKeyId = credentials.awsAccessKeyId
    date = credentials.date
  in
    accessKeyId ++ "/" ++ date ++ "/us-east-1/s3/aws4_request"

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
    Http.multipartBody
        [ stringPart "key" nf.name
        , stringPart "x-amz-algorithm" "AWS4-HMAC-SHA256"
        , stringPart "x-amz-credential" "_credential_"
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
      _ = Debug.log "awzCredential = " (awzCredential model result)
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
