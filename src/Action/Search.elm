module Action.Search exposing (..)

import Types exposing (..)


updateSearch : Model -> String -> ( Model, Cmd Msg )
updateSearch model searchTerms =
    ( { model | searchTerms = searchTerms }, Cmd.none )
