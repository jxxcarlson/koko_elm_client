module Document.Document exposing (..)

import Types exposing (Document)


hasTag : String -> Document -> Bool
hasTag tagg document =
    List.any (\x -> x == tagg) document.tags
