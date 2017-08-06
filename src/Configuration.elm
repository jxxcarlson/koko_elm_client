module Configuration exposing(..)

-- host : String
-- host =
--   "http://localhost:4000"
-- client : String
-- client = "localhost:3000"
--
-- websocketHost : String
-- websocketHost =
--   "ws://localhost:4000/socket/websocket"
--
-- tickInterval : Float
-- tickInterval = 1000.0

host : String
host =
  "https://nshost.herokuapp.com"
  -- "https://mysterious-forest-36511.herokuapp.com"

client : String
client = "http://www.knode.io"


websocketHost : String
websocketHost =
  "wss://mysterious-forest-36511.herokuapp.com/socket/websocket"

tickInterval : Float
tickInterval = 1.0
