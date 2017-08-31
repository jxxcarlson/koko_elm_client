module Document.MasterDocument exposing(
    addTo
  , attach
  , select
  , setParentId
  )

import Types exposing(
  ActiveDocumentList(..)
  , Document
  , Model
  , Msg
  , Page(EditorPage)
  , SearchDomain(All)
  , SearchOrder(Alphabetical)
  , Tool(TableOfContents)
  )

import Action.Document exposing(saveDocumentCmd, hasId)
import Document.Search as Search exposing(withCommand, onEnter)
import Document.Stack as Stack
import Request.Document


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

        appState = model.appState

        newAppState = { appState | masterDocLoaded = True, activeDocumentList = SearchResultList }

        searchState =
            model.searchState

        updatedSearchState =
            { searchState | query = "master=" ++ (toString document_id) }

        updatedModel =
            { model | searchState = updatedSearchState, appState = newAppState }
        (model1, cmd1) = Search.onEnter model.searchState.domain 13 updatedModel
        (model2, cmd2) = Action.Document.selectDocument model1 document
    in
        (model2, Cmd.batch[cmd1, cmd2])


setParentId : String -> Model -> (Model, Cmd Msg)
setParentId parentIdString model =
  let
    document = model.current_document
    newDocument = {document | parent_id = String.toInt parentIdString |> Result.withDefault 0 }
  in
  ({model | current_document = newDocument, message = "parent = " ++ parentIdString}, Cmd.none)

addTo : Model -> (Model, Cmd Msg)
addTo model =
  let
    _ = Debug.log "addTo" model.master_document.id
    appState = model.appState
    newAppState = { appState | tool = TableOfContents }
    query = "id=" ++ (toString model.master_document.id)
    cmds = [
         saveDocumentCmd model.appState.command model.master_document model
         --, update model model.master_document
       , Search.withCommand query Alphabetical All EditorPage model

    ]
  in
    ({model | appState = newAppState,  message = model.appState.command},
    Cmd.batch cmds)



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
