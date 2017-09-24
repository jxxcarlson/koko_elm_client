module Document.Document exposing (..)

import Types exposing (Document)
import Regex
import String.Extra


hasTag : String -> Document -> Bool
hasTag tagg document =
    List.any (\x -> x == tagg) document.tags
