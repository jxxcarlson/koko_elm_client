module Document.Dictionary exposing (..)

import Types exposing (Document, DocumentDict)
import Dict


empty : DocumentDict
empty =
    Dict.empty


insert : String -> Document -> DocumentDict -> DocumentDict
insert key document dict =
    Dict.insert key document dict


set : String -> Document -> DocumentDict -> DocumentDict
set key document dict =
    let
        dict2 =
            if member key dict then
                remove key dict
            else
                dict
    in
        insert key document dict2


remove : String -> DocumentDict -> DocumentDict
remove key dict =
    Dict.remove key dict


member : String -> DocumentDict -> Bool
member key dict =
    Dict.member key dict


get : String -> DocumentDict -> Maybe Document
get key dict =
    Dict.get key dict
