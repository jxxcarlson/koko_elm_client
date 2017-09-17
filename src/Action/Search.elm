module Action.Search exposing (..)

import Types exposing (Model, SearchDomain(..), SearchState, Msg(..), SearchOrder(..), ActiveDocumentList(..))
import Document.Search


selectSearchMode : String -> Model -> ( Model, Cmd Msg )
selectSearchMode searchMode model =
    let
        domain =
            case searchMode of
                "private" ->
                    Private

                "public" ->
                    Public

                "all" ->
                    All

                _ ->
                    Public

        oldSearchState =
            model.searchState

        newSearchState =
            if model.appState.signedIn then
                { oldSearchState | domain = domain }
            else
                { oldSearchState | domain = Public }
    in
        ( { model | searchState = newSearchState }, Cmd.none )


selectSearchOrder : String -> Model -> ( Model, Cmd Msg )
selectSearchOrder searchOrder model =
    let
        order =
            case searchOrder of
                "viewed" ->
                    Viewed

                "created" ->
                    Created

                "alpha" ->
                    Alphabetical

                _ ->
                    Viewed

        oldSearchState =
            model.searchState

        newSearchState =
            { oldSearchState | order = order }
    in
        ( { model | searchState = newSearchState }, Cmd.none )


doSearch : SearchDomain -> Int -> Model -> ( Model, Cmd Msg )
doSearch searchDomain key model =
    let
        _ =
            Debug.log "Action.Search.doSearch with query" model.searchQueryInputBuffer

        searchState =
            model.searchState

        newSearchState =
            { searchState | query = model.searchQueryInputBuffer }

        appState =
            model.appState

        newAppState =
            { appState | activeDocumentList = SearchResultList }

        newModel =
            { model | searchState = newSearchState, appState = newAppState }
    in
        Document.Search.onEnter searchDomain key newModel
