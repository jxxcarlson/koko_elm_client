module User.Display exposing(..)


import Types exposing (Model, Msg, User, Users)

import StyleSheet exposing (..)
import Color
import Element as EL exposing (..)
import Element.Attributes as EA exposing (..)
import Element.Events as EE exposing (..)

import Utility
import FontAwesome
import StyleSheet exposing (..)



list : String -> Model -> Element Styles variation Msg
list title model =
    column None [height (percent 100)] [
         userListHeader title model
         , userListView1 title model
       ]


userListView1 title model =
   column PaleBlue [ yScrollbar, paddingTop 15, spacing 0, width (px 300), height (px (toFloat (model.window.height - 200))) ]
    (List.map (viewTitle model) model.userList)


userListHeader : String -> Model -> Element Styles variation Msg
userListHeader title model =
  el HeadingAlternate [ height (px 30), paddingXY 8 4 ] (text "Users")

viewTitle : Model -> User -> Element Styles variation Msg
viewTitle model user =
  (el None [moveDown 15.0] (text user.username))
