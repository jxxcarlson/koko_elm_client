module Document.Search
    exposing
        ( withCommand
        , getRandomDocuments
        , recallLastSearch
        , onEnter
        , update
        , updateDomain
        , withParameters
        )

import Types
    exposing
        ( ActiveDocumentList(SearchResultList)
        , defaultMasterDocument
        , defaultDocument
        , Model
        , Msg
        , Page(..)
        , SearchDomain(..)
        , SearchOrder(..)
        , SearchState
        , Tool(..)
        )
import Action.UI
import Request.Document
import Document.RenderAsciidoc as RenderAsciidoc


{-

   Search methods.

-}


withParameters : String -> SearchOrder -> SearchDomain -> Page -> Model -> ( Model, Cmd Msg )
withParameters query order domain page model =
    let
        newSearchState =
            SearchState query domain order

        newModel =
            { model | searchState = newSearchState }
    in
        withModel page newModel


{-| Execute search stored in model.searchState and display results in Page.
All searches should be run through this function.
-}
withModel : Page -> Model -> ( Model, Cmd Msg )
withModel page model =
    let
        masterDocLoaded_ =
            if String.contains "master" model.searchState.query then
                True
            else
                False

        _ =
            Debug.log "In Search.withModel, masterDocLoaded_" masterDocLoaded_

        appState =
            model.appState

        newAppState =
            { appState
                | masterDocLoaded = masterDocLoaded_
                , tool = TableOfContents
                , page = page
            }

        updatedModel =
            { model
                | appState = newAppState
                , master_document = defaultMasterDocument
                , documents2 = model.documents
            }
    in
        ( { updatedModel | appState = newAppState }
        , Cmd.batch
            [ Request.Document.getDocumentsWith model.searchState model.current_user.token
            , RenderAsciidoc.put model.current_document
            ]
        )



-- XXX


search : SearchDomain -> String -> Page -> Model -> ( Model, Cmd Msg )
search searchDomain query page model =
    let
        _ =
            Debug.log "Document.search, query" query

        query2 =
            if query == "" then
                case searchDomain of
                    Public ->
                        "random=public"

                    Private ->
                        "random_user=" ++ (toString model.current_user.id)

                    All ->
                        "random=all"
            else
                query

        _ =
            Debug.log "Document.search, query2" query2

        searchState =
            model.searchState

        newSearchState =
            { searchState | domain = searchDomain, query = query2 }

        newModel =
            { model | searchState = newSearchState }
    in
        withModel page newModel


withCommand : String -> SearchOrder -> SearchDomain -> Page -> Model -> Cmd Msg
withCommand query order domain page model =
    let
        newSearchState =
            SearchState query domain order

        newModel =
            { model | searchState = newSearchState }
    in
        Request.Document.getDocumentsWith model.searchState model.current_user.token



---------- Below the line --------


onEnter : SearchDomain -> Int -> Model -> ( Model, Cmd Msg )
onEnter searchDomain key model =
    if (Debug.log "key" key) == 13 then
        let
            _ =
                Debug.log "Firing Action.Document.onEnter" 1

            searchState =
                updatedSearchState model searchDomain
        in
            search searchState.domain searchState.query (Action.UI.displayPage model) model
    else
        ( model, Cmd.none )


recallLastSearch : Model -> ( Model, Cmd Msg )
recallLastSearch model =
    let
        appState =
            model.appState

        newAppState =
            { appState | masterDocLoaded = False, tool = TableOfContents }
    in
        ( { model
            | documents = model.documents2
            , current_document = List.head model.documents2 |> Maybe.withDefault defaultDocument
            , appState = newAppState
            , master_document = defaultMasterDocument
            , message = "Set masterDocLoaded: False"
          }
        , Cmd.none
        )


cleanQuery : String -> String
cleanQuery query =
    String.split "&" query
        |> List.filter (\item -> not (String.contains "random" item))
        |> String.join "&"


getRandomDocuments : Model -> ( Model, Cmd Msg )
getRandomDocuments model =
    let
        _ =
            Debug.log "IN getRandomDocuments, searchDomain is" model.searchState.domain

        appState =
            model.appState

        newAppState =
            { appState | page = ReaderPage, activeDocumentList = SearchResultList }

        newModel =
            { model | appState = newAppState }

        initialQuery =
            cleanQuery model.searchState.query

        randomQuery =
            case model.searchState.domain of
                All ->
                    "random=all&user_id=" ++ (toString model.current_user.id)

                Public ->
                    "random=public"

                Private ->
                    "random_user=" ++ (toString model.current_user.id)

        query =
            if initialQuery == "" then
                randomQuery
            else
                randomQuery ++ "&" ++ initialQuery
    in
        withParameters query Alphabetical model.searchState.domain ReaderPage newModel



{-
   UPDATERS: Thes updated the search parameters stored in model.searchState
-}


update : Model -> String -> ( Model, Cmd Msg )
update model query =
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


updateDomain : Model -> SearchDomain -> ( Model, Cmd Msg )
updateDomain model searchDomain =
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
