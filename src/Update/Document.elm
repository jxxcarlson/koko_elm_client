module Update.Document exposing (update)

import Action.Document
import Action.Error
import Document.Dictionary
import Document.Document as Document
import Document.MasterDocument
import Document.Render
import Document.Search
import Document.TOC
import Request.Document
import Types exposing (DeleteState(..), DocMsg(..), Page(..), SearchDomain(..), SearchOrder(..))
import Utility


update submessage model =
    case submessage of
        RandomDocuments ->
            Document.Search.getRandomDocuments model

        DoRender key ->
            Document.Render.putWithKey key model

        GetRenderedText str ->
            let
                document =
                    model.current_document

                newDocument =
                    { document | rendered_content = str }

                newModel =
                    { model | current_document = newDocument }

                _ =
                    Debug.log "GetRenderedText" "now"
            in
            -- Action.Document.saveCurrentDocument "" newModel
            ( { model | current_document = newDocument }, Cmd.none )

        EditSpecialDocument ->
            let
                appState =
                    model.appState

                newAppState =
                    { appState | page = EditorPage }
            in
            ( { model | current_document = model.specialDocument, appState = newAppState }
            , Cmd.none
            )

        GetDocuments (Ok documentsRecord) ->
            Action.Document.updateDocuments model documentsRecord

        GetDocuments (Err error) ->
            ( { model | message = Action.Error.httpErrorString error }, Cmd.none )

        GetUserDocuments (Ok documentsRecord) ->
            Action.Document.updateDocuments model documentsRecord

        GetUserDocuments (Err error) ->
            ( { model | message = Action.Error.httpErrorString error }, Cmd.none )

        GetSpecialDocument (Ok documentsRecord) ->
            let
                specialDocument =
                    case List.head documentsRecord.documents of
                        Just document ->
                            document

                        Nothing ->
                            Document.emptyDocument
            in
            ( { model | specialDocument = specialDocument }, Cmd.none )

        GetSpecialDocument (Err err) ->
            ( { model | message = "Getting special document: error" }, Cmd.none )

        SetDocumentInDict (Ok ( documentsRecord, key )) ->
            let
                document =
                    case List.head documentsRecord.documents of
                        Just document ->
                            document

                        Nothing ->
                            Document.emptyDocument

                documentDict =
                    model.documentDict

                newDocumentDict =
                    if document /= Document.emptyDocument then
                        Document.Dictionary.set key document documentDict
                    else
                        documentDict
            in
            ( { model | documentDict = newDocumentDict }, Cmd.none )

        SetDocumentInDict (Err err) ->
            ( { model | message = "Error setting key in documentDict" }, Cmd.none )

        GetMasterDocument (Ok documentsRecord) ->
            let
                masterDocument =
                    case List.head documentsRecord.documents of
                        Just document ->
                            document

                        Nothing ->
                            Document.emptyDocument

                oldDocuments =
                    model.documents

                newDocuments =
                    Utility.replaceIf (Action.Document.hasId masterDocument.id) masterDocument oldDocuments
            in
            ( { model | master_document = masterDocument }, Cmd.none )

        GetMasterDocument (Err err) ->
            ( { model | message = "Getting master document: error" }, Cmd.none )

        PutDocument (Ok serverReply) ->
            case serverReply of
                () ->
                    ( model, Cmd.none )

        PutDocument (Err error) ->
            ( { model | message = Action.Error.httpErrorString error }, Cmd.none )

        NewDocument ->
            let
                newDocument =
                    Document.defaultDocument
            in
            Action.Document.createDocument model Document.blankDocument

        NewDiaryEntry ->
            let
                newDocument =
                    Document.diaryEntry
            in
            Action.Document.createDocument model (Document.diaryEntry model.date)

        GetDiary ->
            Document.Search.withParameters "key=diary" Created Private ReaderPage model

        AddToMasterDocument ->
            let
                _ =
                    Debug.log "MAIN: AddToMasterDocument" "now"
            in
            Document.MasterDocument.addTo model

        --( model , Request.Document.createDocument newDocument model.current_user.token )
        AttachCurrentDocument location ->
            let
                appState =
                    model.appState

                newAppState =
                    { appState | command = Document.MasterDocument.attach location model }
            in
            ( { model | appState = newAppState }, Cmd.none )

        CreateDocument (Ok documentRecord) ->
            Action.Document.selectNewDocument model documentRecord.document

        CreateDocument (Err error) ->
            ( { model | message = Action.Error.httpErrorString error }, Cmd.none )

        RequestDocumentDelete ->
            let
                appState =
                    model.appState

                newAppState =
                    { appState | deleteState = Pending }
            in
            ( { model | appState = newAppState }, Cmd.none )

        CancelDocumentDelete ->
            let
                appState =
                    model.appState

                newAppState =
                    { appState | deleteState = Resting }
            in
            ( { model | appState = newAppState }, Cmd.none )

        DeleteCurrentDocument ->
            let
                appState =
                    model.appState

                newAppState =
                    { appState | deleteState = Resting }
            in
            ( { model
                | appState = newAppState
                , message = "Delete current document"
              }
            , Request.Document.deleteCurrentDocument model
            )

        DeleteDocument serverReply ->
            Action.Document.deleteDocument serverReply model

        RenumberDocuments ->
            Document.TOC.renumberMasterDocument model

        Title title ->
            Action.Document.setTitle title model

        SetTextType textType ->
            Action.Document.setTextType textType model

        SetDocType docType ->
            Action.Document.setDocType docType model

        SetParentId parentIdString ->
            Document.MasterDocument.setParentId parentIdString model

        InputTags tagString ->
            Action.Document.updateTags tagString model

        SaveCurrentDocument ->
            let
                _ =
                    Debug.log "SaveCurrentDocument" "now"
            in
            Action.Document.saveCurrentDocument "" model

        SaveDocument result ->
            ( { model | message = "Document saved" }, Cmd.none )

        AdoptChildren ->
            let
                _ =
                    Debug.log "AdoptChildren" "now"
            in
            Action.Document.saveCurrentDocument "adopt_children=yes" model

        SelectDocument document ->
            let
                _ =
                    Debug.log "SelectDocument" "now"
            in
            Action.Document.selectDocument model document

        SelectMaster document ->
            Document.MasterDocument.select document model

        InputContent content ->
            Action.Document.inputContent content model

        UpdateDocument ->
            let
                _ =
                    Debug.log "UpdateDocument" "now"
            in
            Action.Document.updateCurrentDocumentWithContent model

        LatexFullRender ->
            let
                _ =
                    Debug.log "UpdateDocument" "now"
            in
            Action.Document.latexFullRender model

        TogglePublic ->
            Action.Document.togglePublic model

        UpdateTextInputBuffer str ->
            ( { model | textInputBuffer = str }, Cmd.none )

        MigrateFromAsciidocLatex ->
            Action.Document.migrateFromAsciidocLatex model
