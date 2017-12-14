module Action.Document
    exposing
        ( clearEditRecord
        , createDocument
        , deleteDocument
        , hasId
        , inputContent
        , latexFullRender
        , migrateFromAsciidocLatex
        , saveCurrentDocument
        , saveDocumentCmd
        , selectDocument
        , selectNewDocument
        , setDocType
        , setTextType
        , setTitle
        , togglePublic
        , toggleUpdateRate
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
import Document.Stack as Stack
import External exposing (putTextToRender, toJs)
import MiniLatex.Driver
import Random
import Regex
import Request.Document
import String.Extra
import Task
import Types exposing (..)
import Utility exposing (replaceIf)
import Views.External exposing (windowData)


---( { model | documentStack = documentsRecord.documents }, Cmd.none )


clearEditRecord : AppState -> AppState
clearEditRecord appState =
    let
        newEditRecord =
            MiniLatex.Driver.emptyEditRecord
    in
    { appState | editRecord = newEditRecord }



--|> Debug.log "processed latex"


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


latexFullRender : Model -> ( Model, Cmd Msg )
latexFullRender model =
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
            MiniLatex.Driver.update model.appState.seed MiniLatex.Driver.emptyEditRecord document.content

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


macros : DocumentDict -> String
macros documentDict =
    if Dictionary.member "texmacros" documentDict then
        Dictionary.getContent "texmacros" documentDict
            |> Regex.replace Regex.All (Regex.regex "\n+") (\_ -> "\n")
            |> String.Extra.replace "$$" "\n$$\n"
    else
        ""


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
                , Task.attempt GetUserDocuments (saveTask |> Task.andThen (\_ -> refreshMasterDocumentTask))
                ]
            else
                [ Render.put False model.appState.editRecord.idList model.appState.textBufferDirty document -- put new content in JS-mirror of document and save the document (XX: client-server)
                , Task.attempt SaveDocument saveTask
                , External.saveUserState (Data.User.encodeUserState newModel)
                , Random.generate NewSeed (Random.int 1 10000)
                ]
    in
    ( newModel, Cmd.batch cmds )


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
        ]
    )


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


hasId : Int -> Document -> Bool
hasId id document =
    document.id == id


wordCount : Document -> Int
wordCount document =
    document.content
        |> String.split " "
        |> List.length


getDocumentsById : Model -> Int -> List Document
getDocumentsById model k =
    List.filter (\doc -> doc.id == k) model.documents


getDocumentById : Model -> Int -> Maybe Document
getDocumentById model k =
    List.head (getDocumentsById model k)


createDocument : Model -> Document -> ( Model, Cmd Msg )
createDocument model document =
    let
        appState =
            model.appState

        newAppState =
            { appState | tool = NewDocumentTools, page = EditorPage }
    in
    ( { model | appState = newAppState }, Request.Document.createDocument document model.current_user.token )


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


{-| Tags the form k:v define a key-value pair.
The function extractValue key taglist resturns
the value of the key-value pair as a Maybe Int
-}
texmacroFileId : List String -> Int
texmacroFileId tags =
    let
        maybeMacrotag =
            tags
                |> List.filter (\tag -> String.startsWith "texmacros:" tag)
                |> List.head

        fileId =
            case maybeMacrotag of
                Nothing ->
                    0

                Just tag ->
                    fileIdHelper tag
    in
    fileId


fileIdHelper : String -> Int
fileIdHelper tag =
    let
        maybeIdString =
            tag |> String.split ":" |> List.drop 1 |> List.head

        id =
            case maybeIdString of
                Nothing ->
                    0

                Just idString ->
                    idString |> String.toInt |> Result.withDefault 0
    in
    id


selectDocument : Model -> Document -> ( Model, Cmd Msg )
selectDocument model document =
    let
        appState =
            if (model.appState.page == EditorPage) && (document.attributes.textType == "latex") then
                model.appState |> clearEditRecord
            else
                model.appState

        macroFileId =
            texmacroFileId document.tags |> toString

        newAppState =
            { appState
                | editRecord = MiniLatex.Driver.emptyEditRecord
                , masterDocLoaded = masterDocLoaded model document
                , masterDocOpened = masterDocOpened model document
                , page = displayPage model
                , textBufferDirty = False
            }

        newModel =
            { model
                | current_document = document
                , documentStack = Stack.push document model.documentStack
                , appState = newAppState
                , counter = model.counter + 1
            }

        basicCommands =
            [ toJs (windowData newModel (displayPage model))
            , Render.put False newModel.appState.editRecord.idList model.appState.textBufferDirty document
            , External.saveUserState (Data.User.encodeUserState newModel)
            ]

        additionalCommands =
            if newModel.appState.page == EditorPage && document.attributes.textType == "latex" then
                [ Dictionary.setItemInDict ("id=" ++ macroFileId) "texmacros" newModel.current_user.token ]
            else
                []
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
        doc =
            model.current_document

        new_document =
            { doc | title = title }
    in
    updateCurrentDocument model new_document


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
