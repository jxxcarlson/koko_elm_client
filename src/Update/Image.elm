module Update.Image exposing (update)

import External
import Image.Upload
import Types exposing (Model, ImageMsg(..), Msg(ImageMsg))


update : ImageMsg -> Model -> (Model, Cmd Msg)
update submessage model =
    case submessage of
        ImageSelected ->
            ( { model | message = "Image selected" }
            , External.fileSelected (Debug.log "IMAGE SELECTED" model.imageRecord.id)
            )

        ImageRead data ->
            let
                newImage =
                    { contents = data.contents
                    , filename = data.filename
                    }

                newImageRecord =
                    { id = "ImageInputId", mImage = Just newImage }
            in
            ( { model | imageRecord = newImageRecord }
            , Cmd.none
            )

        GetUploadCredentials ->
            Image.Upload.getUploadCredentials model

        CredentialsResult (Ok result) ->
           let 
             _ = Debug.log "Credentials Result" result
           in
            Image.Upload.request result.credentials model

        CredentialsResult (Err error) ->
            let
              _ = Debug.log("CredentialsResult") error
            in
            ( model, Cmd.none )

        Files nativeFiles ->
            ( { model | fileToUpload = List.head nativeFiles }, Cmd.none )

        -----
        UploadComplete (Ok result) ->
            ( model, Cmd.none )

        UploadComplete (Err error) ->
           let 
             _ = Debug.log "ERROR" error
           in
            ( {model | message = "ERROR: " ++ (toString error)}  , Cmd.none )

        FileSelected ->
            ( model, External.fileUpload model.fileInputId )

        FileUploaded True ->
            -- obviously, set some state notifying success
            ( model, Cmd.none )

        FileUploaded False ->
            -- obviously, set some state notifying failure
            ( model, Cmd.none )

-- errorMessage error =
--   case error of 
--         BadStatus status -> status.code |> toString 
--     _ -> "Some other error"

  