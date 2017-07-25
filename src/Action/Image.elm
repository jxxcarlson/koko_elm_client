module Action.Image exposing(..)

import Types exposing(Model, Image, defaultImage)

getUploadCredentials model =
  let
    image = model.imageRecord.mImage |> Maybe.withDefault defaultImage
  in
     ( { model | message = "image: " ++ image.filename }, Cmd.none)
