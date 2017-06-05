module Views.Component exposing (..)

import Html exposing (..)
import Html.Attributes as HA exposing (..)
import Html.Events as HE exposing (onClick)
import Utility exposing (styles)
import Types exposing (..)
import Css exposing (..)


selectedClass : Page -> Model -> String
selectedClass page model =
    if page == model.page then
        "isSelected"
    else
        "isNotSelected"


pageSelector : Model -> Html Msg
pageSelector model =
    span [ styles [ Css.marginLeft (Css.px 100.0) ] ]
        [ button [ onClick (GoTo HomePage), HA.class (selectedClass HomePage model) ]
            [ Html.text "Home" ]
        , button
            [ onClick (GoTo ReaderPage), HA.class (selectedClass ReaderPage model) ]
            [ Html.text "Reader" ]
        , button
            [ onClick (GoTo EditorPage), HA.class (selectedClass EditorPage model) ]
            [ Html.text "Editor" ]
        ]
