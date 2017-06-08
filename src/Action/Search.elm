module Action.Search exposing (..)

import Types exposing (..)


updateSearch : Model -> String -> ( Model, Cmd Msg )
updateSearch model query =
    let
        searchState =
            model.searchState

        new_searchState =
            { searchState | query = query }
    in
        ( { model | searchState = new_searchState }, Cmd.none )


updateSearchDomain : Model -> SearchDomain -> ( Model, Cmd Msg )
updateSearchDomain model searchDomain =
    let
        searchState =
            model.searchState

        new_searchState =
            { searchState | domain = searchDomain }
    in
        ( { model | searchState = new_searchState }, Cmd.none )
