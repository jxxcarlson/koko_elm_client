module User.Synchronize exposing (..)

import Data.User
import Document.Document exposing (pageNotFoundDocument)
import Document.Render as Render
import External
import Http
import MiniLatex.Driver
import Request.Document
import Task exposing (Task)
import Types exposing (ActiveDocumentList(..), DocumentsRecord, Model, Msg(..), Page(..), UserStateRecord)


{-| Recover state from local storage
-}
doRecoverUserState : String -> Model -> ( Model, Cmd Msg )
doRecoverUserState jsonString model =
    let
        maybeUserStateRecord =
            Data.User.decodeUserStateRecord jsonString
    in
    case maybeUserStateRecord of
        Ok userStateRecord ->
            let
                appState =
                    model.appState

                newAppState =
                    { appState | page = ReaderPage, activeDocumentList = DocumentStackList }

                token =
                    userStateRecord.token

                docStackTask =
                    recoverDocumentStackTask userStateRecord token

                currentDocTask =
                    recoverCurrentDocumentTask userStateRecord token

                innerSetUserStateTask =
                    Task.map2 (\a b -> ( a, b )) currentDocTask docStackTask
            in
            ( { model | appState = newAppState }
            , Cmd.batch
                [ Task.attempt SetUserState innerSetUserStateTask
                , Request.Document.getDocumentWithAuthenticatedQuery
                    GetSpecialDocument
                    token
                    "key=sidebarNotes"
                ]
            )

        Err error ->
            ( { model | warning = "Sorry, I cannot recover your user state" }, Cmd.none )


setUserStateTask : UserStateRecord -> String -> Task Http.Error ( DocumentsRecord, DocumentsRecord )
setUserStateTask userStateRecord token =
    let
        docStackTask =
            recoverDocumentStackTask userStateRecord token

        currentDocTask =
            recoverCurrentDocumentTask userStateRecord token
    in
    Task.map2 (\a b -> ( a, b )) currentDocTask docStackTask


setUserState data model =
    let
        currentDocument =
            data
                |> Tuple.first
                |> .documents
                |> List.head
                |> Maybe.withDefault pageNotFoundDocument

        documentList =
            data
                |> Tuple.second
                |> .documents

        appState =
            model.appState

        newAppState =
            { appState
                | editRecord = MiniLatex.Driver.emptyEditRecord
                , textBufferDirty = False
            }

        newModel =
            { model
                | current_document = currentDocument
                , documentStack = documentList
                , appState = newAppState
            }

        saveUserStateToLocalStorageCommand =
            External.saveUserState (Data.User.encodeUserState newModel)

        renderCommand =
            Render.put False newModel.appState.editRecord.idList model.appState.textBufferDirty currentDocument
    in
    ( newModel
    , Cmd.batch [ saveUserStateToLocalStorageCommand, renderCommand ]
    )



-- Task.attempt GetUserDocuments (saveTask |> Task.andThen (\_ -> refreshMasterDocumentTask))


recoverCurrentDocumentTask userStateRecord token =
    let
        queryForCurrentDocument =
            case userStateRecord.currentDocumentId of
                Ok currentDocumentId ->
                    Debug.log "xxxx queryForCurrentDocument"
                        ("id=" ++ toString currentDocumentId ++ "&docs=any")

                Err err ->
                    Debug.log "xxxx error queryForCurrentDocument"
                        "id=316"
    in
    Request.Document.getDocumentWithAuthenticatedQueryTask token queryForCurrentDocument


recoverDocumentStackTask userStateRecord token =
    let
        idList1 =
            userStateRecord.documentIntStack |> List.map toString |> String.join ","

        idList =
            if idList1 == "" then
                "316"
            else
                idList1

        queryForDocumentStack =
            "idlist=" ++ idList
    in
    Request.Document.getDocumentWithAuthenticatedQueryTask token queryForDocumentStack



-- |> Http.toTask


{-| Set the currentDocumen using the data in the DocumentsRecord.
-}
setCurrentDocument : DocumentsRecord -> Model -> ( Model, Cmd Msg )
setCurrentDocument documentsRecord model =
    let
        maybeCurrentDocument =
            documentsRecord.documents |> List.head

        _ =
            Debug.log "xxxx setCurrentDocument" maybeCurrentDocument

        currentDocument =
            case maybeCurrentDocument of
                Just doc ->
                    doc

                Nothing ->
                    pageNotFoundDocument

        appState =
            model.appState

        newAppState =
            { appState | page = ReaderPage }

        _ =
            Debug.log "IN setCurrentDocument, title = " currentDocument.title

        newModel =
            { model | current_document = currentDocument, documents = documentsRecord.documents }

        cmd =
            External.saveUserState (Data.User.encodeUserState newModel)
    in
    ( newModel, Cmd.none )


{-| Set documentStack using the data in the DocumentsRecord.
-}
loadDocumentStack : DocumentsRecord -> Model -> ( Model, Cmd Msg )
loadDocumentStack documentsRecord model =
    let
        documents =
            documentsRecord.documents

        newModel =
            { model | documents = documents }

        appState =
            model.appState

        newAppState =
            { appState | page = ReaderPage }

        cmd =
            External.saveDocumentStack (Data.User.encodeDocumentStack newModel)

        _ =
            Debug.log "xxx number of documents" (List.length documents)
    in
    --( { model | documentStack = documents }, Cmd.none )
    ( { newModel | documentStack = documents }, cmd )
