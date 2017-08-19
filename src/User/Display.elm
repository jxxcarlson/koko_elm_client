module User.Display exposing(..)


import Types exposing (
     Model
    , Msg(GetPublicPage, GetHomePageForUserHomePages)
    , User
    , Users
    , Page(..)
    , SearchDomain(..)
    , SearchOrder(..)
  )

import StyleSheet exposing (..)
import Color
import Element as EL exposing (..)
import Element.Attributes as EA exposing (..)
import Element.Events as EE exposing (..)

import Utility
import FontAwesome
import StyleSheet exposing (..)
import Document.Search

import Views.Common as Common



list : String -> Model -> Element Styles variation Msg
list title model =
    column None [height (percent 100)] [
         userListHeader title model
         , userListView1 title model
       ]


userListView1 title model =
   column PaleBlue [ yScrollbar, paddingTop 15, spacing 0, width (px 300), height (px (toFloat (model.window.height - 200))) ]
    (List.map (viewUser model) model.userList)


userListHeader : String -> Model -> Element Styles variation Msg
userListHeader title model =
  el HeadingAlternate [ height (px 30), paddingXY 8 4 ] (text "Users")

viewUser : Model -> User -> Element Styles variation Msg
viewUser model user =
  let
    query =  "authorname=" ++ user.username ++ "&key=home"
  in
    (el (titleStyle model user) [
         onClick (GetHomePageForUserHomePages query user.username)
         , spacing 15
         , paddingLeft 10
         , height (px 25)
         , width (px 500)
      ]
      (text (Common.shortString 35 (user.username ++ ": " ++ user.blurb))))

titleStyle : Model -> User -> Styles
titleStyle model user =
    if model.selectedUserName == user.username then
      Blue
    else
      PaleBlue
