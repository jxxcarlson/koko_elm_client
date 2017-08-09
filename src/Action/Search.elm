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


updatedSearchState : Model -> SearchDomain -> SearchState
updatedSearchState model searchDomain =
    let
        searchState =
            model.searchState

        newSearchDomain =
            if model.current_user.token /= "" then
                searchState.domain
            else
                Public

    in
        { searchState | domain = newSearchDomain }


updateSearchDomain : Model -> SearchDomain -> ( Model, Cmd Msg )
updateSearchDomain model searchDomain =
    let
        searchState =
            model.searchState

        newSearchDomain =
            if searchDomain == Private && model.current_user.token /= "" then
                Private
            else if searchDomain == Public then
                Public
            else
                Public

        newMessage =
            if searchDomain == Private && model.current_user.token /= "" then
                "Sorry, you must sign in to search for private documents"
            else
                model.message

        new_searchState =
            { searchState | domain = newSearchDomain }
    in
        ( { model | searchState = new_searchState, message = newMessage }, Cmd.none )
