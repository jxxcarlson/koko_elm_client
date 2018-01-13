module Document.Search
    exposing
        ( getRandomDocuments
        , onEnter
        , recallLastSearch
        , search
        , update
        , updateDomain
        , withParameters
        )

import Action.UI
import Document.Document
import Document.Query as Query
import Document.Render as Render
import Http
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
   getRandomDocuments : Model -> ( Model, Cmd Msg )
   onEnter : SearchDomain -> Int -> Model -> ( Model, Cmd Msg )
   recallLastSearch : Model -> ( Model, Cmd Msg )
   search : SearchState -> Model -> ( Model, Cmd Msg )
   update : Model -> String -> ( Model, Cmd Msg )
   updateDomain : Model -> SearchDomain -> ( Model, Cmd Msg )
   withParameters : String -> SearchOrder -> SearchDomain -> Page -> Model -> ( Model, Cmd Msg )

   CMD MSG:
   dispatch : SearchState -> Page -> Model -> ( Model, Cmd Msg )
   getDocuments : SearchState -> Int -> String -> Cmd Msg

   HELPERS:
   fixDomain : SearchState -> Model -> SearchDomain
   processorAndRoute : SearchDomain -> ( Result Http.Error Types.DocumentsRecord -> DocMsg, String )

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


search : SearchState -> Model -> ( Model, Cmd Msg )
search searchState model =
    withParameters searchState.query searchState.order searchState.domain model.appState.page model


withParameters : String -> SearchOrder -> SearchDomain -> Page -> Model -> ( Model, Cmd Msg )
withParameters query order domain page model =
    let
        _ =
            Debug.log "withParameters" ( query, order, domain )

        searchState =
            SearchState query domain order
    in
    dispatch searchState page model


onEnter : SearchDomain -> Int -> Model -> ( Model, Cmd Msg )
onEnter searchDomain key model =
    if Debug.log "key" key == 13 then
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
            fixQueryIfEmpty searchState.query searchState.domain model

        domain =
            fixDomain searchState model

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



-- XXX


fixQueryIfEmpty : String -> SearchDomain -> Model -> String
fixQueryIfEmpty query searchDomain model =
    if query == "" then
        case searchDomain of
            Public ->
                "random=public"

            Private ->
                "random_user=" ++ toString model.current_user.id

            All ->
                "random=all"
    else
        query


fixDomain : SearchState -> Model -> SearchDomain
fixDomain searchState model =
    if model.current_user.token == "" then
        Public
    else
        searchState.domain


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
    withParameters query Alphabetical model.searchState.domain ReaderPage newModel



{-
   UPDATERS: These update the search parameters stored in model.searchState
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


getDocuments : SearchState -> Int -> String -> Cmd Msg
getDocuments searchState user_id token =
    let
        searchDomain =
            if token == "" then
                Public
            else
                searchState.domain

        ( processor, route ) =
            Query.processorAndRoute searchDomain

        adjustedQuery =
            Query.makeQuery searchState searchDomain user_id

        searchTask =
            Request.Document.getDocumentsTask route adjustedQuery token
    in
    Task.attempt (DocMsg << GetUserDocuments) (searchTask |> Task.andThen (\documentsRecord -> refreshMasterDocumentTask route token documentsRecord))



-- (Result Http.Error DocumentsRecord)
-- (Result Http.Error DocumentsRecord -> msg)
-- getDocuments : String -> String -> Result Http.Error DocumentsRecord -> String -> Cmd msg
