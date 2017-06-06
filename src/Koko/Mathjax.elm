module Koko.Mathjax exposing (toHtml)

import Native.Mathjax
import Html exposing (Attribute, Html)


toHtml : List (Attribute msg) -> String -> Html msg
toHtml attrs string =
    Native.Mathjax.toHtml attrs string



-- ERROR: VM204 app.js:12828 Uncaught TypeError: Cannot read property 'elm_event_node_ref' of undefined
