module Views.Image exposing(..)

import Json.Decode
import StyleSheet exposing (..)
import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events exposing (..)
import Views.Component as Component
import Types exposing (..)
import Html
import Html.Attributes as HA
import Html.Events as HE

imageEditor : Model -> List (Element Styles variation Msg)
imageEditor model =
  [ namedGrid Container
      { columns = [ px 300, fill 1, fill 1 ]
      , rows =
          [ px 1 => [ spanAll "separator" ]
          , px 40 => [ span 1 "Header1", span 1 "Header2", span 1 "Header3" ]
          , px 650 => [ span 1 "Content1", span 1 "Content2", span 1 "Content3" ]
          , px 40 => [ spanAll "footer" ]
          ]
      }
      []
      [ named "separator" (hairline Hairline)
      , named "footer" (Component.footer model)
      , named "Content2" (imageUploadPane model)
      ]
  ]


imageUploadPane : Model -> Element Styles variation Msg
imageUploadPane model =
  column None [spacing 20] [
    imagePane model
    , uploadImageButton model
  ]

imagePane : Model -> Element Styles variation Msg
imagePane model =
    let
        imagePreview =
            case model.imageRecord.mImage of
                Just i ->
                     viewImagePreview i
                Nothing ->
                    viewImagePreview defaultImage
    in
        row None [ width (px 450)]
          [ html (chooseImageButton model imagePreview)
          ]

chooseImageButton : Model -> Html.Html Msg -> Html.Html Msg
chooseImageButton model imagePreview =
  Html.div [ HA.class "imageWrapper" ]
            [ Html.input
                [ HA.type_ "file"
                , HA.id model.imageRecord.id
                , HE.on "change"
                    (Json.Decode.succeed ImageSelected)
                ]
                []
            , imagePreview
            ]

uploadImageButton : Model -> Element Styles variation Msg
uploadImageButton model =
  el FlatButton
      [ width (px 200)
      , onClick NoOp
      , height (px 30)
      , verticalCenter
      ]
      (el Zero [center, verticalCenter] (text ("Upload image")))

viewImagePreview : Image -> Html.Html Msg
viewImagePreview img =
    Html.img
        [ HA.src img.contents
        , HA.title img.filename
        , HA.style
          [ ("margin-top", "20px")
          , ("backgroundColor", "black")
          , ("height", "200px")
          ]
        ]
        []

-- fileUploadPanel : Model -> Html.Html Msg
-- fileUploadPanel model =
--     Html.div []
--         [ Html.label [ HA.class "btn btn-primary" ]
--             [ Html.input
--                 [ HA.type_ "file"
--                 , HA.id model.fileInputId
--                 , HA.style [ ( "display", "none" ) ]
--                 , HE.on "change" (Json.Decode.succeed FileSelected)
--                 ]
--                 []
--             , Html.text "Upload"
--             ]
--         ]
