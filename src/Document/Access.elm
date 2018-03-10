module Document.Access exposing(..)

import Document.Document as Document
import Types exposing(Document, AccessDict)
import Dict exposing(Dict)
import Element exposing (..)
import Element.Attributes exposing (..)
import StyleSheet exposing (..)



accessString : AccessDict -> String -> String 
accessString dict key =
  case Dict.get key dict of 
    Just value ->
      key ++ ": " ++ value 
    Nothing ->
      key ++ ":"

accessElement dict key =
  el Small [height (px 15)] (text (accessString dict key))   

accessElementList document =
  let 
    dict = document.access
    keys = Dict.keys dict
  in 
    keys |> List.foldl (\key acc -> acc ++ [accessElement dict key]) []

