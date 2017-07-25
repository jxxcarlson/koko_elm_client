module Action.Image exposing(..)

-- import Types exposing(Model, Image, defaultImage, Credentials, CredentialsResult)
import Types exposing(..)
import Json.Decode exposing(field)

import Http

getUploadCredentials model =
  let
    image = model.imageRecord.mImage |> Maybe.withDefault defaultImage
    url = "http://localhost:4000/api/file_upload_presigned?filename=" ++ image.filename ++ "&mimetype=image/jpeg"
    cmd = Http.get url decodeCredentialsWrapper
      |> Http.send CredentialsResult
  in
     ( { model | message = "image: " ++ image.filename }, cmd)

-- app.js:20576 XMLHttpRequest cannot load localhost:4000/api/file_upload_presigned?chem_lab.jpg&mimetype=image/jpeg.
-- Cross origin requests are only supported for protocol schemes: http, data, chrome, chrome-extension, https.

    -- documentRecordDecoder =
    --     JPipeline.decode DocumentRecord
    --         |> JPipeline.required "document" (documentDecoder)

decodeCredentials : Json.Decode.Decoder Credentials
decodeCredentials =
    Json.Decode.map5 Credentials
        (field "signature" Json.Decode.string)
        (field "policy" Json.Decode.string)
        (field "key" Json.Decode.string)
        (field "acl" Json.Decode.string)
        (field "AWSAccessKeyId" Json.Decode.string)

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
