module Document.Search
    exposing
        ( getDocumentsAndContent
        , getRandomDocuments
        , recallLastSearch
        , searchOnEnter
        , searchWithParameters
        , searchWithSearchState
        , update
        , updateDomain
        )

import Action.UI
import Document.Document
import Document.Query as Query
import Document.Render as Render
import Request.Document
import Task
import Types
    exposing
        ( ActiveDocumentList(SearchResultList)
        , DocMsg(..)
        , Model
        , Msg(..)
        , Page(..)
        , SearchDomain(..)
        , SearchOrder(..)
        , SearchState
        , Tool(..)
        )


{- FUNCTIONS

   API:

   searchOnEnter : SearchDomain -> Int -> Model -> ( Model, Cmd Msg )
   searchWithSearchState : SearchState -> Model -> ( Model, Cmd Msg )
   searchWithParameters : String -> SearchOrder -> SearchDomain -> Page -> Model -> ( Model, Cmd Msg )

   recallLastSearch : Model -> ( Model, Cmd Msg )

   getRandomDocuments : Model -> ( Model, Cmd Msg )

   update : Model -> String -> ( Model, Cmd Msg )
   updateDomain : Model -> SearchDomain -> ( Model, Cmd Msg )

   NOTE: searchOnEnter, searchWithParameters, and searchWithSearchState all call dispatchSearch


   CMD MSG:
   dispatchSearch : SearchState -> Page -> Model -> ( Model, Cmd Msg )
   getDocuments : SearchState -> Int -> String -> Cmd Msg

   HELPERS:
   processorAndRoute : SearchDomain -> ( Result Http.Error Types.DocumentsRecord -> DocMsg, String )

   Search methods.

   Main:
   UserHomePage, GetPublicPage, GetHomePageForUserHomePages, UserHomePage  => searchWithParameters
   =============================
   Document.Search:
     searchWithParameters, searchOnEnter >>> dispatchSearch
     ----------------------------------
     getDocuments searchState user_id token
     Note: user_id can be 0 (empty current_user)
   =========================
   Request.Document.getDocuments route query message token

-}


searchWithSearchState : SearchState -> Model -> ( Model, Cmd Msg )
searchWithSearchState searchState model =
    searchWithParameters searchState.query searchState.order searchState.domain model.appState.page model


searchWithParameters : String -> SearchOrder -> SearchDomain -> Page -> Model -> ( Model, Cmd Msg )
searchWithParameters query order domain page model =
    let
        _ =
            Debug.log "searchWithParameters" ( query, order, domain )

        searchState =
            SearchState query domain order
    in
    dispatchSearch searchState page model


searchOnEnter : SearchDomain -> Int -> Model -> ( Model, Cmd Msg )
searchOnEnter searchDomain key model =
    if Debug.log "key" key == 13 then
        let
            _ =
                Debug.log "Firing Action.Document.searchOnEnter" 1

            searchState =
                model.searchState

            newSearchState =
                { searchState | domain = searchDomain }
        in
        dispatchSearch newSearchState (Action.UI.displayPage model) model
    else
        ( model, Cmd.none )


{-| Execute searchWithSearchState stored in model.searchState and display results in Page.
All searches should be run through this function.
-}
dispatchSearch : SearchState -> Page -> Model -> ( Model, Cmd Msg )
dispatchSearch searchState page model =
    let
        masterDocLoaded_ =
            if String.contains "master" searchState.query then
                True
            else
                False

        appState =
            model.appState

        newAppState =
            { appState
                | masterDocLoaded = masterDocLoaded_
                , tool = TableOfContents
                , activeDocumentList = SearchResultList
                , page = page
            }

        query =
            Query.fixQueryIfEmpty searchState.query searchState.domain model.current_user.id

        domain =
            makeSureSearchDomainIsAuthorized2 searchState model.current_user.token

        order =
            searchState.order

        newSearchState =
            SearchState query domain order

        updatedModel =
            { model
                | appState = newAppState
                , master_document = Document.Document.defaultMasterDocument
                , searchState = newSearchState
                , documents2 = model.documents
            }
    in
    ( updatedModel
    , Cmd.batch
        [ getDocuments updatedModel.searchState model.current_user.id model.current_user.token
        , Render.put False model.appState.editRecord.idList model.appState.textBufferDirty model.current_document
        ]
    )


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
        , current_document = List.head model.documents2 |> Maybe.withDefault Document.Document.pageNotFoundDocument
        , appState = newAppState
        , master_document = Document.Document.defaultMasterDocument
        , message = "Set masterDocLoaded: False"
      }
    , Cmd.none
    )


{-| Return list of random documents
-}
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
            Query.cleanQuery model.searchState.query

        randomQuery =
            case model.searchState.domain of
                All ->
                    "random=all&user_id=" ++ toString model.current_user.id

                Public ->
                    "random=public"

                Private ->
                    "random_user=" ++ toString model.current_user.id

        query =
            if initialQuery == "" then
                randomQuery
            else
                randomQuery ++ "&" ++ initialQuery
    in
    searchWithParameters query Alphabetical model.searchState.domain ReaderPage newModel



{-
   UPDATERS: These update the searchWithSearchState parameters stored in model.searchState
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
            makeSureSearchDomainIsAuthorized model searchDomain

        newMessage =
            unauthorizedSearchMessage searchDomain model

        new_searchState =
            { searchState | domain = newSearchDomain }
    in
    ( { model | searchState = new_searchState, message = newMessage }, Cmd.none )


makeSureSearchDomainIsAuthorized : Model -> SearchDomain -> SearchDomain
makeSureSearchDomainIsAuthorized model searchDomain =
    if searchDomain == Private && model.current_user.token /= "" then
        Private
    else if searchDomain == Public then
        Public
    else
        Public


unauthorizedSearchMessage : SearchDomain -> Model -> String
unauthorizedSearchMessage searchDomain model =
    if searchDomain == Private && model.current_user.token /= "" then
        "Sorry, you must sign in to searchWithSearchState for private documents"
    else
        model.message



{- REQUEST THE DOCUMENTS: HERE IS WHERE THE ACTION HAPPENS -}


getDocuments : SearchState -> Int -> String -> Cmd Msg
getDocuments searchState user_id token =
    let
        searchDomain =
            makeSureSearchDomainIsAuthorized2 searchState token

        ( processor, route ) =
            Debug.log "processor and route"
                (Query.processorAndRoute searchDomain)

        adjustedQuery =
            Query.makeQuery searchState searchDomain user_id

        searchTask1 =
            Request.Document.getDocumentsTask route (adjustedQuery ++ "&loading") token

        searchTask =
            Request.Document.getDocumentsTask route adjustedQuery token
    in
    Task.attempt (DocMsg << GetUserDocuments) (searchTask1 |> Task.andThen (\documentsRecord -> refreshMasterDocumentTask route token documentsRecord))


getDocumentsAndContent : SearchState -> Int -> String -> Cmd Msg
getDocumentsAndContent searchState user_id token =
    let
        searchDomain =
            makeSureSearchDomainIsAuthorized2 searchState token

        ( processor, route ) =
            Debug.log "processor and route"
                (Query.processorAndRoute searchDomain)

        adjustedQuery =
            Query.makeQuery searchState searchDomain user_id

        searchTask =
            Request.Document.getDocumentsTask route adjustedQuery token
    in
    Task.attempt (DocMsg << GetContent) (searchTask |> Task.andThen (\documentsRecord -> refreshMasterDocumentTask route token documentsRecord))



-- Task.attempt (DocMsg << GetUserDocuments) (searchTask1 |> Task.andThen (\documentsRecord -> refreshMasterDocumentTask route token documentsRecord))


makeSureSearchDomainIsAuthorized2 : SearchState -> String -> SearchDomain
makeSureSearchDomainIsAuthorized2 searchState token =
    if token == "" then
        Public
    else
        searchState.domain


{-| Called by getDocuments
-}
refreshMasterDocumentTask route token documentsRecord =
    let
        documents =
            documentsRecord.documents

        maybeFirstDocument =
            List.head documents

        ( isMasterDocument, masterDocumentId ) =
            case maybeFirstDocument of
                Just document ->
                    ( document.attributes.docType == "master", document.id )

                Nothing ->
                    ( False, 0 )

        task =
            if (List.length documents == 1) && (isMasterDocument == True) then
                Request.Document.getDocumentsTask route ("master=" ++ toString masterDocumentId) token
            else
                Task.succeed documentsRecord
    in
    task



-- (Result Http.Error DocumentsRecord)
-- (Result Http.Error DocumentsRecord -> msg)
-- getDocuments : String -> String -> Result Http.Error DocumentsRecord -> String -> Cmd msg
