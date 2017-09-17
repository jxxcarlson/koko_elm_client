module Document.Search
    exposing
        ( dispatch
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
        , Msg(..)
        , Page(..)
        , SearchDomain(..)
        , SearchOrder(..)
        , SearchState
        , Tool(..)
        )
import Action.UI
import Http
import Request.Document
import Document.QueryParser exposing (parseQuery)
import Document.RenderAsciidoc as RenderAsciidoc


{-

   Search methods.

   Main:
   UserHomePage, GetPublicPage, GetHomePageForUserHomePages, UserHomePage  => withParameters
   =============================
   Document.Search:
     withParameters, onEnter >>> dispatch
     ----------------------------------
     getDocuments searchState user_id token
     Note: user_id can be 0 (empty current_user)
   =========================
   Request.Document.getDocuments route query message token

-}


withParameters : String -> SearchOrder -> SearchDomain -> Page -> Model -> ( Model, Cmd Msg )
withParameters query order domain page model =
    let
        searchState =
            SearchState query domain order
    in
        dispatch searchState page model


onEnter : SearchDomain -> Int -> Model -> ( Model, Cmd Msg )
onEnter searchDomain key model =
    if (Debug.log "key" key) == 13 then
        let
            _ =
                Debug.log "Firing Action.Document.onEnter" 1

            searchState =
                model.searchState

            newSearchState =
                { searchState | domain = searchDomain }
        in
            dispatch newSearchState (Action.UI.displayPage model) model
    else
        ( model, Cmd.none )


{-| Execute search stored in model.searchState and display results in Page.
All searches should be run through this function.
-}
dispatch : SearchState -> Page -> Model -> ( Model, Cmd Msg )
dispatch searchState page model =
    let
        masterDocLoaded_ =
            if String.contains "master" model.searchState.query then
                True
            else
                False

        _ =
            Debug.log "In Search.dispatch, masterDocLoaded_" masterDocLoaded_

        appState =
            model.appState

        newAppState =
            { appState
                | masterDocLoaded = masterDocLoaded_
                , tool = TableOfContents
                , page = page
            }

        query =
            fixQueryIfEmpty searchState.query searchState.domain model

        domain =
            fixDomain model

        order =
            searchState.order

        newSearchState =
            SearchState query domain order

        updatedModel =
            { model
                | appState = newAppState
                , master_document = defaultMasterDocument
                , searchState = newSearchState
                , documents2 = model.documents
            }
    in
        ( updatedModel
        , Cmd.batch
            [ getDocuments model.searchState model.current_user.id model.current_user.token
            , RenderAsciidoc.put model.current_document
            ]
        )



-- XXX


fixDomain : Model -> SearchDomain
fixDomain model =
    if model.current_user.token == "" then
        Public
    else
        model.searchState.domain


fixQueryIfEmpty : String -> SearchDomain -> Model -> String
fixQueryIfEmpty query searchDomain model =
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



{- Code below was moved from Request.Document -}


getDocuments : SearchState -> Int -> String -> Cmd Msg
getDocuments searchState user_id token =
    let
        _ =
            Debug.log "IN getDocuments, searchState is" searchState

        searchDomain =
            if token == "" then
                Public
            else
                searchState.domain

        ( message, route ) =
            messageRoute searchDomain

        _ =
            Debug.log "Firing search ...., order " searchState.order
    in
        Request.Document.getDocuments route (makeQuery searchState searchDomain user_id) message token


makeQuery : SearchState -> SearchDomain -> Int -> String
makeQuery searchState updatedSearchDomain user_id =
    let
        basicQuery =
            -- if searchState.query == "" then
            --     "publicdocs=all"
            -- else
            parseQuery (searchState.query)

        soq =
            searchOrderQuery searchState.order

        prefix =
            case ( updatedSearchDomain, searchState.query ) of
                ( All, "" ) ->
                    "random=all"

                ( Public, "" ) ->
                    "random=public"

                ( Private, "" ) ->
                    "random_user=" ++ (toString user_id)

                ( All, _ ) ->
                    "docs=any"

                ( _, _ ) ->
                    ""

        queryList =
            [ prefix ] ++ [ parseQuery (searchState.query), soq ]
    in
        buildQuery queryList


messageRoute : SearchDomain -> ( Result Http.Error Types.DocumentsRecord -> Msg, String )
messageRoute searchDomain =
    case searchDomain of
        Public ->
            ( GetDocuments, "public/documents" )

        Private ->
            ( GetUserDocuments, "documents" )

        All ->
            ( GetUserDocuments, "documents" )


searchOrderQuery : SearchOrder -> String
searchOrderQuery searchOrder =
    case searchOrder of
        Viewed ->
            "sort=viewed"

        Updated ->
            "sort=updated"

        Created ->
            "sort=created"

        Alphabetical ->
            "sort=title"


buildQuery : List String -> String
buildQuery queryParts =
    queryParts
        |> List.filter (\x -> x /= "")
        |> String.join "&"



-- (Result Http.Error DocumentsRecord)
-- (Result Http.Error DocumentsRecord -> msg)
-- getDocuments : String -> String -> Result Http.Error DocumentsRecord -> String -> Cmd msg
