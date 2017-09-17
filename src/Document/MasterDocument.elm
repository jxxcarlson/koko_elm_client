module Document.MasterDocument
    exposing
        ( addTo
        , attach
        , select
        , setParentId
        )

import Types
    exposing
        ( ActiveDocumentList(..)
        , Document
        , Model
        , Msg(..)
        , Page(EditorPage)
        , SearchDomain(All)
        , SearchOrder(Alphabetical)
        , Tool(TableOfContents)
        )
import Action.Document exposing (saveDocumentCmd, hasId)
import Document.Search as Search exposing (onEnter)
import Document.Stack as Stack
import Request.Document
import Task


select : Document -> Model -> ( Model, Cmd Msg )
select document model =
    if document.attributes.docType == "master" then
        selectAux document.id document model
    else if document.parent_id /= 0 then
        selectAux document.parent_id document model
    else
        ( model, Cmd.none )


selectAux : Int -> Document -> Model -> ( Model, Cmd Msg )
selectAux document_id document model =
    let
        _ =
            Debug.log "Enter Document.MasterDocument.selectAux for id = " document_id

        appState =
            model.appState

        newAppState =
            { appState
                | masterDocLoaded = True
                , masterDocOpened = True
                , activeDocumentList = SearchResultList
            }

        searchState =
            model.searchState

        updatedSearchState =
            { searchState | query = "master=" ++ (toString document_id) }

        updatedModel =
            { model | searchState = updatedSearchState, appState = newAppState }

        ( model1, cmd1 ) =
            Search.onEnter model.searchState.domain 13 updatedModel

        ( model2, cmd2 ) =
            Action.Document.selectDocument model1 document
    in
        ( model1, Cmd.batch [ cmd1, cmd2 ] )


setParentId : String -> Model -> ( Model, Cmd Msg )
setParentId parentIdString model =
    let
        document =
            model.current_document

        newDocument =
            { document | parent_id = String.toInt parentIdString |> Result.withDefault 0 }
    in
        ( { model | current_document = newDocument, message = "parent = " ++ parentIdString }, Cmd.none )



-- addTo2 : Model -> ( Model, Cmd Msg )


addTo : Model -> ( Model, Cmd Msg )
addTo model =
    let
        _ =
            Debug.log "addTo" model.master_document.id

        _ =
            Debug.log "In addTo, command" model.appState.command

        appState =
            model.appState

        newAppState =
            { appState | tool = TableOfContents, masterDocLoaded = True, masterDocOpened = True }

        route =
            "documents"

        query =
            "master=" ++ (toString model.master_document.id)

        saveTask =
            Request.Document.saveDocumentTask model.appState.command model.master_document model

        refreshMasterDocumentTask =
            Request.Document.getDocumentsTask route query model.current_user.token
    in
        ( { model | appState = newAppState, message = model.appState.command }
          -- , Cmd.batch [ cmd1 ]
        , Task.attempt GetUserDocuments (saveTask |> Task.andThen (\_ -> refreshMasterDocumentTask))
        )



-- https://spin.atomicobject.com/2016/10/11/elm-chain-http-requests/


attach : String -> Model -> String
attach location model =
    "attach="
        ++ location
        ++ "&child="
        ++ (toString model.current_document.id)
        ++ "&current="
        ++ (toString (Stack.top 1 model.documentStack).id)


update : Model -> Document -> Cmd Msg
update model masterDocument =
    Request.Document.reloadMasterDocument masterDocument.id model.current_user.token
