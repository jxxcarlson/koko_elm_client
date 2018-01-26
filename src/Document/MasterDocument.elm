module Document.MasterDocument
    exposing
        ( addTo
        , attach
        , prepareExportLatexFromMaster
        , select
        , setParentId
        )

import Document.MiniLatex
import Document.Stack as Stack
import MiniLatex.FastExportToLatex as FastExportToLatex
import MiniLatex.RenderLatexForExport
import MiniLatex.Source as Source
import Request.Document
import Task
import Types
    exposing
        ( ActiveDocumentList(..)
        , DocMsg(..)
        , Document
        , Model
        , Msg(..)
        , Page(EditorPage)
        , SearchDomain(All)
        , SearchOrder(Alphabetical)
        , Tool(TableOfContents)
        )


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
        appState =
            model.appState

        newAppState =
            { appState
                | masterDocLoaded = True
                , masterDocOpened = True
                , activeDocumentList = SearchResultList
            }

        query1 =
            "master=" ++ toString document_id ++ "&loading"

        query2 =
            "master=" ++ toString document_id

        searchState =
            model.searchState

        newSearchState =
            { searchState | query = query2 }

        updatedModel =
            { model | appState = newAppState, searchState = newSearchState }

        task2 =
            if model.appState.signedIn then
                Task.attempt (DocMsg << GetDocuments) (Request.Document.getDocumentsTask "documents" query2 token)
            else
                Task.attempt (DocMsg << GetDocuments) (Request.Document.getDocumentsTask "public/documents" query2 token)

        token =
            model.current_user.token

        cmd =
            if model.appState.signedIn then
                Task.attempt (DocMsg << GetUserDocuments) (Request.Document.getDocumentsTask "documents" query1 token)
            else
                Task.attempt (DocMsg << GetUserDocuments) (Request.Document.getDocumentsTask "public/documents" query1 token)

        commands =
            [ cmd ]
    in
    ( updatedModel, Cmd.batch commands )


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
        appState =
            model.appState

        newAppState =
            { appState | tool = TableOfContents, masterDocLoaded = True, masterDocOpened = True }

        route =
            "documents"

        query =
            "master=" ++ toString model.master_document.id

        saveTask =
            Request.Document.saveDocumentTask model.appState.command model.master_document model

        refreshMasterDocumentTask =
            Request.Document.getDocumentsTask route query model.current_user.token
    in
    ( { model | appState = newAppState, message = model.appState.command }
      -- , Cmd.batch [ cmd1 ]
    , Task.attempt (DocMsg << GetUserDocuments) (saveTask |> Task.andThen (\_ -> refreshMasterDocumentTask))
    )



-- https://spin.atomicobject.com/2016/10/11/elm-chain-http-requests/


attach : String -> Model -> String
attach location model =
    "attach="
        ++ location
        ++ "&child="
        ++ toString model.current_document.id
        ++ "&current="
        ++ toString (Stack.top 1 model.documentStack).id


update : Model -> Document -> Cmd Msg
update model masterDocument =
    Request.Document.reloadMasterDocument masterDocument.id model.current_user.token


concatenateText : List Document -> String
concatenateText documentList =
    documentList |> List.foldl (\doc acc -> acc ++ "\n\n" ++ doc.content) ""


prepareExportLatexFromMaster : Model -> ( Model, Cmd Msg )
prepareExportLatexFromMaster model =
    let
        macroDefinitions =
            Document.MiniLatex.macros model.documentDict

        sourceText =
            List.drop 1 model.documents
                |> concatenateText
                -- |> (\x -> "\n\n\\tablefcontents\n\n" ++ x)
                |> FastExportToLatex.export

        textToExport =
            Source.texPrefix ++ "\n\n" ++ macroDefinitions ++ "\n\n" ++ sourceText ++ "\n\n" ++ Source.texSuffix
    in
    ( { model | textToExport = textToExport }, Cmd.none )
