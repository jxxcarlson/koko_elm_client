module Update.Search exposing (update)

import Action.Search
import Document.Search
import Types exposing (SearchDomain(..), SearchMsg(..))
import User.Request


update submessage model =
    case submessage of
        SetSearchTerm searchTerms ->
            Document.Search.update model searchTerms

        UpdateSearchQueryInputBuffer str ->
            ( { model | searchQueryInputBuffer = str }, Cmd.none )

        SelectSearchMode searchMode ->
            Action.Search.selectSearchMode searchMode model

        SelectSearchOrder searchOrder ->
            Action.Search.selectSearchOrder searchOrder model

        ClearSearch ->
            ( { model | searchQueryInputBuffer = "" }, Cmd.none )

        DoSearch searchDomain key ->
            Action.Search.doSearch searchDomain key model

        RecallLastSearch ->
            Document.Search.recallLastSearch model

        SearchForUserHomePages keyCode ->
            if keyCode == 13 then
                let
                    query =
                        "is_user=" ++ model.searchQueryInputBuffer

                    searchState =
                        model.searchState

                    newSearchState =
                        { searchState | domain = Public }
                in
                ( { model | searchState = newSearchState }, User.Request.getList query )
            else
                ( model, Cmd.none )

        UseSearchDomain searchDomain ->
            Document.Search.updateDomain model searchDomain
