module User.Synchronize exposing (..)

import Types exposing (Model, Page(..), Msg(..), ActiveDocumentList(..), DocumentsRecord)
import Request.Document
import Data.User
import Document.Document exposing (pageNotFoundDocument)
import External


{-| Recover state from local storage
-}
doRecoverUserState : String -> Model -> ( Model, Cmd Msg )
doRecoverUserState jsonString model =
    let
        _ =
            Debug.log "xxxx" ("doRecoverUserState: " ++ (toString model.counter))

        _ =
            Debug.log "xxxx in doRecoverUserState, jsonString" jsonString

        maybeUserStateRecord =
            Debug.log "xxxx in doRecoverUserState, maybeUserStateRecord"
                (Data.User.decodeUserStateRecord jsonString)

        _ =
            Debug.log "xxxx maybeUserStateRecord" maybeUserStateRecord
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

                    -- model.current_user.token
                    cmd1 =
                        recoverDocumentStackCmd userStateRecord token

                    cmd2 =
                        recoverCurrentDocumentCmd userStateRecord token
                in
                    ( { model | appState = newAppState }, Cmd.batch [ cmd1, cmd2 ] )

            Err error ->
                ( { model | warning = "Sorry, I cannot recover your user state" }, Cmd.none )


recoverCurrentDocumentCmd userStateRecord token =
    let
        queryForCurrentDocument =
            case userStateRecord.currentDocumentId of
                Ok currentDocumentId ->
                    Debug.log "xxxx queryForCurrentDocument"
                        ("id=" ++ (toString currentDocumentId))

                Err err ->
                    Debug.log "xxxx error queryForCurrentDocument"
                        ("id=316")
    in
        Request.Document.getDocumentWithAuthenticatedQuery SetCurrentDocument token queryForCurrentDocument



-- External.saveUserState (Data.User.encodeUserState newModel)


recoverDocumentStackCmd userStateRecord token =
    let
        idList1 =
            userStateRecord.documentIntStack |> List.map toString |> (String.join ",")

        idList =
            if idList1 == "" then
                "316"
            else
                idList1

        queryForDocumentStack =
            ("idlist=" ++ idList)
    in
        Request.Document.getDocumentWithAuthenticatedQuery LoadDocumentStack token queryForDocumentStack


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



-- External.saveUserState (Data.User.encodeUserState newModel)


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
            External.saveUserState (Data.User.encodeUserState newModel)

        _ =
            Debug.log "xxx number of documents" (List.length documents)
    in
        --( { model | documentStack = documents }, Cmd.none )
        ( { newModel | documentStack = documents }, Cmd.none )
