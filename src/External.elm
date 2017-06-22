port module External exposing (..)


port render : String -> Cmd msg


port toJs : String -> Cmd msg


port persist : String -> Cmd msg
