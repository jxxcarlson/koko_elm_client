module Action.Image exposing(..)

-- import Types exposing(Model, Image, defaultImage, Credentials, CredentialsResult)
import Types exposing(..)
import Json.Decode exposing(field)
import Date.Extra
import Date exposing(Date)

import Http

getUploadCredentials model =
  let
    image = model.imageRecord.mImage |> Maybe.withDefault defaultImage
    url = "http://localhost:4000/api/credentials?filename=" ++ image.filename ++ "&mimetype=image/jpeg&bucket=yada"
    cmd = Http.get url decodeCredentialsWrapper
      |> Http.send CredentialsResult
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


awzCredential : Model -> CredentialsWrapper -> String
awzCredential model credentialsWrapper  =
  let
    accessKeyId = credentialsWrapper.credentials.awsAccessKeyId
    date = credentialsWrapper.credentials.date
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


---

-- uploadRequest : Credentials -> NativeFile -> Request String
-- uploadRequest creds file =
--     Http.request
--         { method = "POST"
--         , headers = []
--         , url = "https://localhost:4000"
--         , body = multiPartBody creds file
--         , expect = Http.expectString
--         , timeout = Nothing
--         , withCredentials = False
--         }
