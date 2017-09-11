module Nav.UrlParseExtra exposing (id)

import Configuration
import Parser exposing (..)


id : Parser Int
id =
    succeed identity
        |. symbol (Configuration.client ++ "/##public/")
        |= int
