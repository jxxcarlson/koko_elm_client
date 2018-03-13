module User.Synchronize exposing (..)

import Data.User
import Document.Dictionary
import Document.Document exposing (pageNotFoundDocument)
import Document.Render as Render
import External
import Http
import Json.Encode
import MiniLatex.Driver
import Request.Document
import Task exposing (Task)
import Types
    exposing
        ( ActiveDocumentList(..)
        , DocMsg(..)
        , Document
        , DocumentsRecord
        , Model
        , Msg(..)
        , Page(..)
        , UserMsg(SetUserState)
        , UserStateRecord
        , InfoForOutside(UserState, SaveDocumentStack)
        )
import Utility.KeyValue as KeyValue
import OutsideInfo


setTexMacroDocument : Model -> Cmd Msg
setTexMacroDocument model =
    let
        maybeTexMacroDocumentID =
            texMacroDocmentID model.current_document

        command =
            case maybeTexMacroDocumentID of
                Just id ->
                    Document.Dictionary.setItemInDict ("id=" ++ toString id) "texmacros" model.current_user.token

                Nothing ->
                    Cmd.none
    in
    command


texMacroDocmentID : Document -> Maybe Int
texMacroDocmentID document =
    KeyValue.getIntValueForKeyFromTagList "texmacros" document.tags


{-| Recover state from local storage
-}
doRecoverUserState : UserStateRecord -> Model -> ( Model, Cmd Msg )
doRecoverUserState userStateRecord model =
    let

        _ = Debug.log "HERE IS" "doRecoverUserState"
        
        _ = Debug.log "doRecoverUserState, userStateRecord" userStateRecord

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
        [ Task.attempt (UserMsg << SetUserState) innerSetUserStateTask
        , Request.Document.getDocumentWithAuthenticatedQuery
            (DocMsg << GetSpecialDocument)
            token
            "key=sidebarNotes"
        ]
    )       

        


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
                , textNeedsUpdate = False
            }

        newModel =
            { model
                | current_document = currentDocument
                , documentStack = documentList
                , appState = newAppState
            }

        saveUserStateToLocalStorageCommand =
            OutsideInfo.sendInfoOutside (UserState <| Data.User.encodeUserStateAsValue newModel)

        renderCommand =
            Render.putTextToRender False newModel.appState.editRecord.idList model.appState.textNeedsUpdate currentDocument
    in
    ( newModel
    , Cmd.batch [ saveUserStateToLocalStorageCommand, renderCommand, setTexMacroDocument newModel ]
    )


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
            OutsideInfo.sendInfoOutside (UserState <| Data.User.encodeUserStateAsValue newModel)
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
            OutsideInfo.sendInfoOutside (SaveDocumentStack <| Data.User.encodeDocumentStack newModel)

        _ =
            Debug.log "xxx number of documents" (List.length documents)
    in
    ( { newModel | documentStack = documents }, cmd )
