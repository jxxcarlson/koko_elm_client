module Configuration exposing (..)


host : String
host =
    "https://nshost.herokuapp.com"


client : String
client =
    "http://www.knode.io"


websocketHost : String
websocketHost =
    "wss://nshost.herokuapp.com/socket/websocket"


tickInterval : Float
tickInterval =
    1.0


tabletWidth : Int
tabletWidth =
    800


phoneWidth : Int
phoneWidth =
    600
