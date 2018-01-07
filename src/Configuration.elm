module Configuration exposing (..)

-- --
-- -- --


host : String
host =
    "http://localhost:4000"


client : String
client =
    "http://localhost:3000"


websocketHost : String
websocketHost =
    "ws://localhost:4000/socket/websocket"


tickInterval : Float
tickInterval =
    60.0


tabletWidth : Int
tabletWidth =
    800


phoneWidth : Int
phoneWidth =
    600
