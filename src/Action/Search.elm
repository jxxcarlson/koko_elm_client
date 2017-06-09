module Action.Search exposing (..)

import Types exposing (..)


parseQuery1 : String -> String
parseQuery1 input =
    input
        |> String.split (" ")
        |> List.map String.trim
        |> List.filter (\item -> item /= "")
        |> List.map (\item -> "title=" ++ item)
        |> String.join ("&")


parseQuery : String -> String
parseQuery input =
    if String.contains input "=" then
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


updateSearchDomain : Model -> SearchDomain -> ( Model, Cmd Msg )
updateSearchDomain model searchDomain =
    let
        searchState =
            model.searchState

        new_searchState =
            { searchState | domain = searchDomain }
    in
        ( { model | searchState = new_searchState }, Cmd.none )
