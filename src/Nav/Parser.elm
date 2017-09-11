module Nav.Parser exposing (urlParser)

import Navigation exposing (..)
import Nav.UrlParser as UrlParser exposing (..)
import Types exposing (Page(..), Msg(..))


-- ROUTING


route : Parser (Page -> a) a
route =
    oneOf
        [ UrlParser.map HomePage (UrlParser.s "home")
        , UrlParser.map PrivatePage (UrlParser.s "document" </> int)
        , UrlParser.map PublicPage (UrlParser.s "public" </> int)
        ]


urlParser : Location -> Msg
urlParser location =
    parseHash2 route location
        |> GoToPage
