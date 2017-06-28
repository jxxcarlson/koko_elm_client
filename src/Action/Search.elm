module Action.Search exposing (..)

import Types exposing (..)
import Regex


parseQuery1 : String -> String
parseQuery1 input =
    input
        |> Regex.split Regex.All (Regex.regex "[, ]")
        |> List.map String.trim
        |> List.filter (\item -> item /= "")
        -- |> List.map (\item -> "title=" ++ item)
        |>
            List.map (\item -> transformItem item)
        |> String.join ("&")


transformItem : String -> String
transformItem item =
    if String.contains ":" item then
        Debug.log "term" (transformQualifiedItem item)
    else
        "title=" ++ item


transformQualifiedItem : String -> String
transformQualifiedItem item =
    case String.split ":" item of
        [ "k", stem ] ->
            "key=" ++ stem

        [ "t", stem ] ->
            "text=" ++ stem

        [ "ti", stem ] ->
            "title=" ++ stem

        [ "p", stem ] ->
            "public=" ++ stem

        [ "a", stem ] ->
            "author=" ++ stem

        _ ->
            ""


parseQuery : String -> String
parseQuery input =
    if String.contains "=" input then
        input
    else if input == "all" then
        input
    else
        parseQuery1 (input)


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
            if searchDomain == Private && model.current_user.token /= "" then
                Private
            else if searchDomain == Public then
                Public
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
