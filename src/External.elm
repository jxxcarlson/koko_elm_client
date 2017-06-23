port module External exposing (..)


port render : String -> Cmd msg


port toJs : String -> Cmd msg


port persist : String -> Cmd msg



-- Ask JS to send back the token, if any, in localStorage:


port askToReconnectUser : String -> Cmd msg


port reconnectUser : (String -> msg) -> Sub msg



-- TEST:


port toElm : (String -> msg) -> Sub msg
