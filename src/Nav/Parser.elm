module Nav.Parser exposing (urlParser)

import Nav.UrlParser as UrlParser exposing (..)
import Navigation exposing (..)
import Types exposing (Msg(..), Page(..), PageMsg(..))


-- ROUTING


route : Parser (Page -> a) a
route =
    oneOf
        [ UrlParser.map StartPage (UrlParser.s "home")
        , UrlParser.map PrivatePage (UrlParser.s "document" </> int)
        , UrlParser.map PublicPage (UrlParser.s "public" </> int)
        ]


urlParser : Location -> Msg
urlParser location =
    parseHash2 route location
        |> (PageMsg << GoToPage)
