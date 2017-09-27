module Document.Dictionary exposing (..)

import Types exposing (Document)
import Regex
import String.Extra


insert key document model =
    Dict.insert key document model.documentDict


update key document model =
    Dict.update key document model.documentDict


remove key model =
    Dict.remove key model.documentDict


member key model =
    Dict.member key model.documentDict


get key model =
    Dict.get key model.documentDict
