module Action.Document
    exposing
        ( clearEditRecord
        , createDocument
        , deleteDocument
        , hasId
        , inputContent
        , latexFullRender
        , loadContent
        , loadContentAndRender
        , migrateFromAsciidocLatex
        , saveCurrentDocument
        , saveDocumentCmd
        , saveDocumentListCmd
        , selectDocument
        , selectNewDocument
        , setDocType
        , setTextType
        , setTitle
        , togglePublic
        , toggleUpdateRate
        , updateContent
        , updateCurrentDocument
        , updateCurrentDocumentWithContent
        , updateDocuments
        , updateTags
        , wordCount
        )

-- import MiniLatex.Driver as MiniLatex
-- import MiniLatex.Differ as Differ exposing (EditRecord)
-- import MiniLatex.LatexState exposing (emptyLatexState)

import Action.UI exposing (appStateWithPage, displayPage, updateToolStatus)
import Data.User
import Document.Dictionary as Dictionary
import Document.Document as Document exposing (defaultDocument, defaultMasterDocument, pageNotFoundDocument)
import Document.Edit
import Document.Render as Render
import Document.Search
import Document.Stack as Stack
import External exposing (putTextToRender, toJs)
import MiniLatex.Driver
import MiniLatex.Source as Source
import Random
import Regex
import Request.Document
import String.Extra
import Task
import Types exposing (..)
import Utility exposing (replaceIf)
import Utility.KeyValue as KeyValue
import Views.External exposing (windowData)


{-

    clearEditRecord : AppState -> AppState

    createDocument : Model -> Document -> ( Model, Cmd Msg )
    deleteDocument : Result a value -> Model -> ( Model, Cmd Msg )

    hasId : Int -> Document -> Bool

    inputContent : String -> Model -> ( Model, Cmd Msg )
    latexFullRender : Model -> ( Model, Cmd Msg )
    macros : DocumentDict -> String

    masterDocLoaded : Model -> Document -> Bool
    masterDocOpened : Model -> Document -> Bool

    migrateFromAsciidocLatex : Model -> ( Model, Cmd Msg )

    parseTagString : String -> List String

    saveCurrentDocument : String -> Model -> ( Model, Cmd Msg )
    saveDocument : String -> Document -> Model -> ( Model, Cmd Msg )
    saveDocumentCmd : String -> Document -> Model -> Cmd Msg
    saveDocumentListCmd : List Document -> Model -> Cmd Msg

    selectDocument : Model -> Document -> ( Model, Cmd Msg )
    selectNewDocument : Model -> Document -> ( Model, Cmd Msg )

    setDocType : String -> Model -> ( Model, Cmd Msg )
    setTextType : String -> Model -> ( Model, Cmd Msg )
    setTitle : String -> Model -> ( Model, Cmd Msg )

    togglePublic : Model -> ( Model, Cmd Msg )
    toggleUpdateRate : Model -> Model

    updateContent : Model -> DocumentsRecord -> ( Model, Cmd Msg )
    updateCurrentDocument : Model -> Document -> ( Model, Cmd Msg )
    updateCurrentDocumentWithContent : Model -> ( Model, Cmd Msg )
    updateCurrentLatexDocumentWithContent : Model -> ( Model, Cmd Msg )
    updateDocuments : Model -> DocumentsRecord -> ( Model, Cmd Msg )
    updateTags : String -> Model -> ( Model, Cmd Msg )

    wordCount : Document -> Int
    ===========
   29


-}
{- LaTeX Processing

   clearEditRecord : AppState -> AppState
   latexFullRender : Model -> ( Model, Cmd Msg )
   macros : DocumentDict -> String

-}


clearEditRecord : AppState -> AppState
clearEditRecord appState =
    let
        newEditRecord =
            MiniLatex.Driver.emptyEditRecord
    in
    { appState | editRecord = newEditRecord }


latexFullRender : Model -> ( Model, Cmd Msg )
latexFullRender model =
    let
        appState =
            model.appState

        document =
            model.current_document

        maybeSectionNumber =
            KeyValue.getIntValueForKeyFromTagList "sectionNumber" document.tags

        sectionNumberCommand =
            case maybeSectionNumber of
                Just id ->
                    "\\setcounter{section}{" ++ toString id ++ "}"

                Nothing ->
                    ""

        enrichedContent =
            sectionNumberCommand ++ "\n\n" ++ document.content

        macroDefinitions =
            let
                macrosString =
                    macros model.documentDict
            in
            macrosString ++ "\n\n$$\n\\newcommand{\\label}[1]{}" ++ "\n$$\n\n"

        newEditRecord =
            MiniLatex.Driver.update model.appState.seed MiniLatex.Driver.emptyEditRecord enrichedContent

        rendered_content =
            MiniLatex.Driver.getRenderedText macroDefinitions newEditRecord

        textToExport =
            Source.texPrefix ++ rendered_content ++ Source.texSuffix

        newAppState =
            { appState | editRecord = newEditRecord }

        newModel =
            { model | appState = newAppState, textToExport = textToExport }

        newDocument =
            { document | rendered_content = rendered_content }
    in
    updateCurrentDocument newModel newDocument


macros : DocumentDict -> String
macros documentDict =
    if Dictionary.member "texmacros" documentDict then
        Dictionary.getContent "texmacros" documentDict
            |> Regex.replace Regex.All (Regex.regex "\n+") (\_ -> "\n")
            |> String.Extra.replace "$$" "\n$$\n"
    else
        ""



{- Update/save document

   inputContent : String -> Model -> ( Model, Cmd Msg )
   updateCurrentDocumentWithContent : Model -> ( Model, Cmd Msg )
   updateCurrentLatexDocumentWithContent : Model -> ( Model, Cmd Msg )
   updateCurrentDocument : Model -> Document -> ( Model, Cmd Msg )
   updateDocuments : Model -> DocumentsRecord -> ( Model, Cmd Msg )
   saveDocument : String -> Document -> Model -> ( Model, Cmd Msg )
   saveDocumentCmd : String -> Document -> Model -> Cmd Msg
   saveDocumentListCmd : List Document -> Model -> Cmd Msg

-}


inputContent : String -> Model -> ( Model, Cmd Msg )
inputContent content model =
    let
        appState =
            model.appState

        newAppState =
            { appState | textBufferDirty = True }

        currentDocument =
            model.current_document

        newCurrentDocument =
            { currentDocument | content = content }
    in
    ( { model | appState = newAppState, current_document = newCurrentDocument }, Cmd.none )


{-| This is the function called when the user changes the document content
in the Editor. It will call updateCurrentDocument which in turn calls
Render.put. This last function pushe the document content to JS-world
for processing by MathJax.
-}
updateCurrentDocumentWithContent : Model -> ( Model, Cmd Msg )
updateCurrentDocumentWithContent model =
    let
        _ =
            Debug.log "updateCurrentDocumentWithContent" "now"
    in
    if model.current_document.attributes.textType == "latex" then
        updateCurrentLatexDocumentWithContent model
    else
        updateCurrentDocument model model.current_document


updateCurrentLatexDocumentWithContent : Model -> ( Model, Cmd Msg )
updateCurrentLatexDocumentWithContent model =
    let
        appState =
            model.appState

        document =
            model.current_document

        macroDefinitions =
            let
                macrosString =
                    macros model.documentDict
            in
            macrosString ++ "\n\n$$\n\\newcommand{\\label}[1]{}" ++ "\n$$\n\n"

        newEditRecord =
            MiniLatex.Driver.update model.appState.seed appState.editRecord document.content

        rendered_content =
            MiniLatex.Driver.getRenderedText macroDefinitions newEditRecord

        newAppState =
            { appState | editRecord = newEditRecord }

        newModel =
            { model | appState = newAppState }

        newDocument =
            { document | rendered_content = rendered_content }
    in
    updateCurrentDocument newModel newDocument


updateCurrentDocument : Model -> Document -> ( Model, Cmd Msg )
updateCurrentDocument model document =
    let
        _ =
            Debug.log "updateCurrentDocument" document.title

        old_documents =
            model.documents

        newDocumentStack =
            Stack.push document model.documentStack

        new_documents =
            Utility.replaceIf (hasId document.id) document old_documents

        appState =
            model.appState

        newAppState =
            { appState | textBufferDirty = False }

        saveTask =
            Request.Document.saveDocumentTask "" document model

        -- XXX: ??? re first argument above Why?
        route =
            "documents"

        query =
            "master=" ++ toString document.id

        refreshMasterDocumentTask =
            Request.Document.getDocumentsTask route query model.current_user.token

        newModel =
            { model
                | current_document = document
                , documents = new_documents
                , documentStack = newDocumentStack
                , lastEditTime = model.time
                , appState = newAppState
            }

        cmds =
            if document.attributes.docType == "master" then
                [ Render.put True model.appState.editRecord.idList True document -- put new content in JS-mirror of document and save the document (XX: client-server)
                , Task.attempt (DocMsg << GetUserDocuments) (saveTask |> Task.andThen (\_ -> refreshMasterDocumentTask))
                ]
            else
                [ Render.put False model.appState.editRecord.idList model.appState.textBufferDirty document -- put new content in JS-mirror of document and save the document (XX: client-server)
                , Task.attempt (DocMsg << SaveDocument) saveTask
                , External.saveUserState (Data.User.encodeUserState newModel)
                , Random.generate (DocMsg << NewSeed) (Random.int 1 10000)
                ]
    in
    ( newModel, Cmd.batch cmds )



-- (a -> msg) -> Generator a -> Cmd msg


updateDocuments : Model -> DocumentsRecord -> ( Model, Cmd Msg )
updateDocuments model documentsRecord =
    let
        _ =
            Debug.log "updateDocuments" "begin"

        current_document =
            case List.head documentsRecord.documents of
                Just document ->
                    document

                Nothing ->
                    pageNotFoundDocument

        masterDocLoaded =
            if current_document.attributes.docType == "master" then
                True
            else
                False

        newMasterDocument =
            if masterDocLoaded then
                current_document
            else
                defaultMasterDocument

        page =
            if model.device == Phone then
                StartPage
            else
                model.appState.page

        docAtTopOfStack =
            Stack.top 0 model.documentStack

        current_document2 =
            if
                masterDocLoaded
                    && docAtTopOfStack.parent_id
                    == newMasterDocument.id
            then
                docAtTopOfStack
            else
                current_document

        appState =
            model.appState

        updatedAppState =
            { appState
                | page = page
                , tool = TableOfContents
                , masterDocLoaded = masterDocLoaded
            }

        newDocumentList =
            documentsRecord.documents

        firstDocument =
            List.head newDocumentList |> Maybe.withDefault defaultDocument

        newDocumentStack =
            if List.length newDocumentList == 1 then
                Stack.push firstDocument model.documentStack
            else
                model.documentStack

        newModel =
            { model
                | documents = newDocumentList
                , master_document = newMasterDocument
                , current_document = current_document2
                , documentStack = newDocumentStack
                , appState = updatedAppState
                , counter = Debug.log "updateDocuments" (model.counter + 1)
            }
    in
    ( newModel
    , Cmd.batch
        [ toJs (windowData model model.appState.page)
        , External.saveUserState (Data.User.encodeUserState newModel)
        , Render.put False model.appState.editRecord.idList newModel.appState.textBufferDirty current_document
        , Document.Search.getDocumentsAndContent newDocumentList model.current_user.id model.current_user.token
        ]
    )


updateContent : Model -> DocumentsRecord -> ( Model, Cmd Msg )
updateContent model documentsRecord =
    let
        documents =
            documentsRecord.documents

        current_document =
            List.head documents |> Maybe.withDefault Document.defaultDocument
    in
    ( { model | message = "Get Content", documents = documents, current_document = current_document }, Cmd.none )


loadContent : Model -> DocumentsRecord -> ( Model, Cmd Msg )
loadContent model documentsRecord =
    let
        documentsFound =
            documentsRecord.documents

        document =
            List.head documentsFound |> Maybe.withDefault Document.defaultDocument

        _ =
            Debug.log "loading document" [ document.id, String.length document.content ]

        documentsInModel =
            model.documents

        new_documents =
            Utility.replaceIf (hasId document.id) document documentsInModel
    in
    ( { model | message = "Get Content", documents = new_documents }, Cmd.none )


loadContentAndRender : Model -> DocumentsRecord -> ( Model, Cmd Msg )
loadContentAndRender model documentsRecord =
    let
        documentsFound =
            documentsRecord.documents

        document =
            List.head documentsFound |> Maybe.withDefault Document.defaultDocument

        documentsInModel =
            model.documents

        new_documents =
            Utility.replaceIf (hasId document.id) document documentsInModel

        _ =
            Debug.log "loadContentAndRender, id to PUT" document.id

        command =
            Render.put True [] False document
    in
    ( { model | message = "Get Content", documents = new_documents, current_document = document }, command )


saveCurrentDocument : String -> Model -> ( Model, Cmd Msg )
saveCurrentDocument queryString model =
    let
        _ =
            Debug.log "saveCurrentDocument" "now"
    in
    saveDocument queryString model.current_document model


saveDocument : String -> Document -> Model -> ( Model, Cmd Msg )
saveDocument queryString document model =
    let
        _ =
            Debug.log "saveDocument" "now"
    in
    ( model, saveDocumentCmd queryString document model )


saveDocumentCmd : String -> Document -> Model -> Cmd Msg
saveDocumentCmd queryString document model =
    let
        _ =
            Debug.log "saveDocumentCmd, id" document.id

        cmd =
            if document.author_id == model.current_user.id then
                Request.Document.put queryString model document
            else
                Cmd.none
    in
    cmd


saveDocumentListCmd : List Document -> Model -> Cmd Msg
saveDocumentListCmd documentList model =
    let
        cmds =
            documentList
                |> List.map (\doc -> saveDocumentCmd "" doc model)
    in
    Cmd.batch cmds



{- User settings

   toggleUpdateRate : Model -> Model
   setTextType : String -> Model -> ( Model, Cmd Msg )
   setDocType : String -> Model -> ( Model, Cmd Msg )
   togglePublic : Model -> ( Model, Cmd Msg )


-}


toggleUpdateRate : Model -> Model
toggleUpdateRate model =
    let
        oldAppState =
            model.appState

        tickerPaused =
            not oldAppState.tickerPaused

        tickInterval =
            if tickerPaused then
                5 * 60.0
            else
                1.0

        newAppState =
            { oldAppState | tickerPaused = tickerPaused, tickInterval = tickInterval }
    in
    { model | appState = newAppState }


setTextType : String -> Model -> ( Model, Cmd Msg )
setTextType textType model =
    let
        oldDocument =
            model.current_document

        docAttributes =
            oldDocument.attributes

        newDocumentAttributes =
            { docAttributes | textType = textType }

        appState =
            model.appState

        newAppState =
            { appState | textTypeMenuDropped = False }

        newModel =
            { model | appState = newAppState }

        -- TEST: foobar = Debug.log "foo" model.current_document.id
        newDocument =
            { oldDocument | attributes = newDocumentAttributes }
    in
    updateCurrentDocument newModel newDocument


setDocType : String -> Model -> ( Model, Cmd Msg )
setDocType docType model =
    let
        oldDocument =
            model.current_document

        docAttributes =
            oldDocument.attributes

        newDocAttributes =
            { docAttributes | docType = docType }

        appState =
            model.appState

        newAppState =
            { appState | docTypeMenuDropped = False }

        newModel =
            { model | appState = newAppState }

        -- TEST: foobar = Debug.log "foo" model.current_document.id
        newDocument =
            { oldDocument | attributes = newDocAttributes }
    in
    updateCurrentDocument newModel newDocument


togglePublic : Model -> ( Model, Cmd Msg )
togglePublic model =
    let
        document =
            model.current_document

        attributes =
            document.attributes

        newAttributes =
            { attributes | public = not attributes.public }

        newDocument =
            { document | attributes = newAttributes }

        updatedModel =
            { model | current_document = newDocument }
    in
    updateCurrentDocument updatedModel newDocument



{- TAGS

   parseTagString : String -> List String
   updateTags : String -> Model -> ( Model, Cmd Msg )



-}


parseTagString : String -> List String
parseTagString str =
    String.split "," str
        |> List.map String.trim


updateTags : String -> Model -> ( Model, Cmd Msg )
updateTags tagText model =
    let
        updatedTags =
            parseTagString tagText

        document =
            model.current_document

        updatedDocument =
            { document | tags = updatedTags }
    in
    ( { model | current_document = updatedDocument }, Cmd.none )


hasId : Int -> Document -> Bool
hasId id document =
    document.id == id


wordCount : Document -> Int
wordCount document =
    document.content
        |> String.split " "
        |> List.length


createDocument : Model -> Document -> ( Model, Cmd Msg )
createDocument model document =
    let
        appState =
            model.appState

        newAppState =
            { appState | tool = NewDocumentTools, page = EditorPage }
    in
    ( { model | appState = newAppState }, Request.Document.createDocument document model.current_user.token )



{- MASTER DOCUMENT

   masterDocLoaded : Model -> Document -> Bool
   masterDocOpened : Model -> Document -> Bool

-}


masterDocLoaded : Model -> Document -> Bool
masterDocLoaded model document =
    if
        document.attributes.docType
            == "master"
            || (document.parent_id
                    == model.master_document.id
                    && document.parent_id
                    /= 0
               )
    then
        True
    else
        False


masterDocOpened : Model -> Document -> Bool
masterDocOpened model document =
    if document.parent_id == model.master_document.id && model.appState.masterDocLoaded then
        True
    else
        False



{- DOCUMENT OPERATIONS

   selectDocument : Model -> Document -> ( Model, Cmd Msg )
   selectNewDocument : Model -> Document -> ( Model, Cmd Msg )
   deleteDocument : Result a value -> Model -> ( Model, Cmd Msg )
   setTitle : String -> Model -> ( Model, Cmd Msg )

-}


selectDocument : Model -> Document -> ( Model, Cmd Msg )
selectDocument model document =
    let
        appState =
            if (model.appState.page == EditorPage) && (document.attributes.textType == "latex") then
                model.appState |> clearEditRecord
            else
                model.appState

        maybeMacroFileId =
            KeyValue.getIntValueForKeyFromTagList "texmacros" document.tags

        newAppState =
            { appState
                | editRecord = MiniLatex.Driver.emptyEditRecord
                , masterDocLoaded = masterDocLoaded model document
                , masterDocOpened = masterDocOpened model document
                , page = displayPage model
                , textBufferDirty = False
            }

        documentAttributes =
            document.attributes

        newDocumentAttributes =
            { documentAttributes | lastViewed = model.time }

        updatedDocument =
            { document | attributes = newDocumentAttributes }

        newModel =
            { model
                | current_document = updatedDocument
                , documentStack = Stack.push document model.documentStack
                , appState = newAppState
                , counter = model.counter + 1
            }

        basicCommands =
            [ toJs (windowData newModel (displayPage model))
            , Render.put False newModel.appState.editRecord.idList model.appState.textBufferDirty document
            , External.saveUserState (Data.User.encodeUserState newModel)
            ]

        setTexMacroFileCmd =
            case maybeMacroFileId of
                Just id ->
                    Dictionary.setItemInDict ("id=" ++ toString id) "texmacros" newModel.current_user.token

                Nothing ->
                    Cmd.none

        additionalCommands =
            [ setTexMacroFileCmd ]
    in
    ( newModel
    , Cmd.batch (basicCommands ++ additionalCommands)
    )


selectNewDocument : Model -> Document -> ( Model, Cmd Msg )
selectNewDocument model document =
    ( { model
        | current_document = document
        , documents = [ document ] ++ model.documents
        , message = "New document added: " ++ document.title
        , counter = model.counter + 1
        , documentStack = Stack.push document model.documentStack
      }
    , Render.put False model.appState.editRecord.idList model.appState.textBufferDirty document
    )


deleteDocument : Result a value -> Model -> ( Model, Cmd Msg )
deleteDocument serverReply model =
    case serverReply of
        Ok serverReply ->
            let
                documents =
                    model.documents

                documentStack =
                    model.documentStack

                updatedDocuments =
                    Utility.removeWhen (\doc -> doc.id == model.current_document.id) documents

                updatedDocumentStack =
                    Utility.removeWhen (\doc -> doc.id == model.current_document.id) documentStack

                newCurrentDocument =
                    List.head updatedDocuments |> Maybe.withDefault Document.errorDocument
            in
            ( { model
                | message = "Document deleted, remaining = " ++ toString (List.length updatedDocuments)
                , documents = updatedDocuments
                , documentStack = updatedDocumentStack
                , current_document = newCurrentDocument
              }
            , Cmd.none
            )

        Err error ->
            ( { model | warning = toString error }, Cmd.none )


setTitle : String -> Model -> ( Model, Cmd Msg )
setTitle title model =
    let
        document =
            model.current_document

        updatedDocument =
            { document | title = title }

        appState =
            model.appState

        newAppState =
            { appState | textBufferDirty = True }
    in
    ( { model | current_document = updatedDocument, appState = newAppState }, Cmd.none )



{-

   TEMPORARY

-}


migrateFromAsciidocLatex : Model -> ( Model, Cmd Msg )
migrateFromAsciidocLatex model =
    let
        currentDocument =
            model.current_document

        updatedContent =
            currentDocument.content
                |> Document.Edit.migrateTextFomAsciidocLaTeX

        updatedDocument =
            { currentDocument | content = updatedContent }

        counter =
            model.counter

        newModel =
            { model | current_document = updatedDocument, counter = counter + 1 }
    in
    updateCurrentDocumentWithContent newModel
