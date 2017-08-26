module Action.Document exposing (..)

import Types exposing (..)
import Utility exposing (replaceIf)
import Request.Document
import External exposing (render)
import Action.UI exposing (displayPage, updateToolStatus, appStateWithPage)
import Views.External exposing (windowData)
import External exposing (toJs)
import Utility
import Action.Search
import Document.Preprocess
import Document.RenderAsciidoc as RenderAsciidoc
import Document.Search
import Document.Stack as Stack

import Document.Search as Search
import Action.Error


{-|
  This is the function called when the user changes the document content
  in the Editorl
-}
updateCurrentDocumentWithContent : String -> Model -> ( Model, Cmd Msg )
updateCurrentDocumentWithContent content model =
    let
        -- _ = Debug.log "updateCurrentDocumentWithContent" 1
        -- _ = Debug.log "CONTENT" content

        -- processed_content = Document.Preprocess.preprocessSource content
        -- _ = Debug.log "Processed CONTENT" processed_content

        oldDocument =
            model.current_document

        -- TEST: foobar = Debug.log "foo" model.current_document.id
        newDocument =
            { oldDocument | content = content, rendered_content = oldDocument.rendered_content   }

    in
        updateCurrentDocument model newDocument

updateCurrentDocument : Model -> Document -> ( Model, Cmd Msg )
updateCurrentDocument model document =
    let
        _ = Debug.log "updateCurrentDocument" 1
        old_documents =
            model.documents

        new_documents =
            Utility.replaceIf (hasId document.id) document old_documents

        appState =
            model.appState

        newAppState =
            { appState | textBufferDirty = False }
    in
        ( { model
            | current_document = document
            , documents = new_documents
            , appState = newAppState
          }
        , Cmd.batch [
            -- put new content in JS-mirror of document and save the document (XX: client-server)
            RenderAsciidoc.put document,
            Request.Document.put "" model document
        ]
        )



toggleUpdateRate : Model -> Model
toggleUpdateRate model =
  let
    oldAppState = model.appState
    tickerPaused = not oldAppState.tickerPaused
    tickInterval = if tickerPaused then
                      5*60.0
                   else
                      1.0

    newAppState = { oldAppState | tickerPaused = tickerPaused, tickInterval = tickInterval}
  in
    { model | appState = newAppState }


-- |> Regex.replace Regex.All (Regex.regex "%.*") (\_ -> "")
-- |> LatexParser.Render.transformText.identity
-- |> Regex.replace Regex.All (Regex.regex "\\label{.*}") (\_ -> "")
-- |> Regex.replace Regex.All (Regex.regex "\\emph{(.*)}") (\{match} -> "<it>\\1</it>")


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
        _ = Debug.log "updateDocuments" "begin"

        current_document =
            case List.head documentsRecord.documents of
                Just document ->
                    document
                Nothing ->
                    defaultDocument

        newMasterDocument = if model.appState.masterDocLoaded then
          current_document
        else
          defaultMasterDocument

        appState =
            model.appState

        updatedAppState =
            { appState | tool = TableOfContents }
    in
        ( { model
            | documents = documentsRecord.documents
            , master_document = newMasterDocument
            , current_document = current_document
            , appState = updatedAppState
            , counter = Debug.log "updateDocuments" (model.counter + 1)
          }
        , Cmd.batch
            [ toJs (windowData model model.appState.page)
            , RenderAsciidoc.put current_document
            ]
        )




saveCurrentDocument : String -> Model -> ( Model, Cmd Msg )
saveCurrentDocument queryString model =
  saveDocument queryString model.current_document model

saveDocument : String -> Document -> Model -> ( Model, Cmd Msg )
saveDocument queryString document model =
    let
      _ = Debug.log "AAA, document, id" document.id
      _ = Debug.log "AAA, document, author_id" document.author_id
      _ = Debug.log "AAA, current user, id" model.current_user.id

      cmd = if document.author_id == model.current_user.id then
          Request.Document.put queryString model document
        else
          Cmd.none
    in
      ( model, cmd )

saveDocumentCmd : String -> Document -> Model -> Cmd Msg
saveDocumentCmd queryString document model =
    let
      _ = Debug.log "BB, saveDocumentCmd, id" document.id
    in
      Request.Document.put queryString model document


hasId : Int -> Document -> Bool
hasId id document =
    document.id == id


wordCount : Document -> Int
wordCount document =
    document.content
        |> String.split (" ")
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
    appState = model.appState
    newAppState = { appState | tool = NewDocumentTools, page = EditorPage}
  in
    ( { model | appState = newAppState }, Request.Document.createDocument document model.current_user.token )


selectDocument : Model -> Document -> ( Model, Cmd Msg )
selectDocument model document =
    let
        appState =
            model.appState

        newAppState =
            { appState | textBuffer = document.content,
            page = displayPage model,
            textBufferDirty = False }
        saveCmd = if document.author_id == model.current_user.id then
             saveDocumentCmd "viewed_at=now" document model
           else
             Cmd.none
    in
        ( { model
            | current_document = document
            , documentStack = Stack.push document model.documentStack
            , appState = newAppState
            , counter = model.counter + 1
          }
        , Cmd.batch
            [ toJs (windowData model (displayPage model))
            , RenderAsciidoc.put document
            , saveCmd
            -- XXX put command here to update doc.viewedAt if it is owned by the user
            ]
        )


selectNewDocument : Model -> Document -> ( Model, Cmd Msg )
selectNewDocument model document =
    ( { model
        | current_document = document
        , documents = [ document ] ++ model.documents
        , message = "New document added: " ++ document.title
        , counter = model.counter + 1
      }
    , RenderAsciidoc.put document
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


searchOnEnter : SearchDomain -> Int -> Model -> ( Model, Cmd Msg )
searchOnEnter searchDomain key model =
    if (Debug.log "key" key) == 13 then
        let
          _ = Debug.log "Firing Action.Document.searchOnEnter" 1
          searchState =
            Action.Search.updatedSearchState model searchDomain
        in
            search searchState.domain searchState.query (displayPage model) model
    else
        ( model, Cmd.none )

-- XXX
search : SearchDomain -> String -> Page -> Model -> ( Model, Cmd Msg )
search searchDomain query page model =
  let
    _ = Debug.log "Firing Action.Document.search" 1
    searchState = model.searchState
    newSearchState = { searchState | domain = searchDomain, query = query }
    newModel = { model | searchState = newSearchState }
  in
    Search.withModel page model


-- search2 : SearchDomain -> String -> Page -> Model -> ( Model, Cmd Msg )
-- search2 searchDomain query page model =
--           let
--               _ = Debug.log "Firing Action.Document.search" 1
--               appState = model.appState
--               newAppState = { appState | masterDocLoaded = False,
--                  tool = TableOfContents,
--                  page = page }
--               oldSearchState = model.searchState
--               newSearchState = {oldSearchState | query = query, domain = searchDomain }
--               updatedModel =
--                   { model
--                       | message = (Action.UI.queryMessage searchDomain) ++ Utility.queryText query
--                       , appState = newAppState
--                       , searchState = newSearchState
--                       , master_document = defaultMasterDocument
--                       , documents2 = model.documents
--                   }
--           in
--               ( { updatedModel | appState = newAppState }
--               , Cmd.batch
--                   [ Request.Document.getDocumentsWith model.searchState model.current_user.token
--                   , RenderAsciidoc.put model.current_document
--                   ]
--               )
--
--


selectMasterDocument : Document -> Model -> ( Model, Cmd Msg )
selectMasterDocument document model =
    if document.attributes.docType == "master" then
      selectMasterDocumentAux document.id model
    else if document.parent_id /= 0 then
      selectMasterDocumentAux document.parent_id model
    else
      ( model, Cmd.none )


selectMasterDocumentAux : Int -> Model -> ( Model, Cmd Msg )
selectMasterDocumentAux document_id model =
    let

        appState = model.appState

        newAppState = { appState | masterDocLoaded = True, activeDocumentList = SearchResultList}

        searchState =
            model.searchState

        updatedSearchState =
            { searchState | query = "master=" ++ (toString document_id) }

        updatedModel =
            { model | searchState = updatedSearchState, appState = newAppState }
    in
        searchOnEnter model.searchState.domain 13 updatedModel

deleteDocument : Result a value -> Model -> (Model, Cmd Msg)
deleteDocument serverReply model =
    case serverReply of
      (Ok serverReply) ->
        let
            documents =
                model.documents

            updatedDocuments =
                Utility.removeWhen (\doc -> doc.id == model.current_document.id) documents

            newCurrentDocument =
                (List.head updatedDocuments) |> Maybe.withDefault Types.defaultDocument
        in
            ( { model
                | message = "Document deleted, remaining = " ++ (toString (List.length updatedDocuments))
                , documents = updatedDocuments
                , current_document = newCurrentDocument
              }
            , Cmd.none
            )
      (Err error) ->
            ( { model | warning = (toString error) }, Cmd.none )

setTitle : String -> Model -> (Model, Cmd Msg)
setTitle title model =
  let
      doc =
          model.current_document

      new_document =
          { doc | title = title }
  in
      updateCurrentDocument model new_document

inputContent : String -> Model -> (Model, Cmd Msg)
inputContent content model =
  let
      appState =
          model.appState

      newAppState =
          { appState | textBuffer = content, textBufferDirty = True }
  in
      ( { model | appState = newAppState }, Cmd.none )

recallLastSearch model =
  let
    appState = model.appState
    newAppState = { appState | masterDocLoaded = False, tool = TableOfContents}
  in
    ( { model | documents = model.documents2,
        current_document = List.head model.documents2 |> Maybe.withDefault defaultDocument,
        appState = newAppState,
        master_document = defaultMasterDocument,
        message = "Set masterDocLoaded: False" }, Cmd.none )

setParentId : String -> Model -> (Model, Cmd Msg)
setParentId parentIdString model =
  let
    document = model.current_document
    newDocument = {document | parent_id = String.toInt parentIdString |> Result.withDefault 0 }
  in
  ({model | current_document = newDocument, message = "parent = " ++ parentIdString}, Cmd.none)


addToMasterDocument model =
  let
    _ = Debug.log "addToMasterDocument" model.master_document.id
    appState = model.appState
    newAppState = { appState | tool = TableOfContents }
    query = "id=" ++ (toString model.master_document.id)
    cmds = [
         Request.Document.put model.appState.command model model.master_document
       , Document.Search.command query Alphabetical All EditorPage model

    ]
  in
    ({model | appState = newAppState,  message = model.appState.command},
    Cmd.batch cmds)



attachDocumentCommand : String -> Model -> String
attachDocumentCommand location model =
  "attach="
      ++ location
      ++ "&child="
      ++ (toString model.current_document.id)
      ++ "&current="
      ++ (toString (Stack.top model.documentStack).id)
