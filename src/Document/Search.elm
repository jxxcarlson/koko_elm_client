module Document.Search exposing(withParameters, withModel, command)

import Types exposing(Model, Msg, SearchState, SearchOrder,
   SearchDomain, Tool(..), Page, defaultMasterDocument)

import Request.Document
import Document.RenderAsciidoc as RenderAsciidoc

withParameters : String -> SearchOrder -> SearchDomain -> Page -> Model -> (Model, Cmd Msg)
withParameters query order domain page model =
  let
    -- _ = Debug.log "Firing Document.Search.withParameters" 1
    newSearchState = SearchState query domain order
    newModel = {model | searchState = newSearchState }
  in
    withModel page newModel
{-|
  Execute search stored in model.searchState and display results in Page.
  All searches should be run through this function.
-}
withModel : Page -> Model -> ( Model, Cmd Msg )
withModel page model =
          let
              _ = Debug.log "Firing Document.Search.withModel" 1
              _ = Debug.log "QQ, Query string" model.searchState.query
              _ = Debug.log "QQ, Search Domain" model.searchState.domain



              masterDocLoaded_ = if String.contains "master" model.searchState.query then
                  True
                else
                  False

              _ = Debug.log "In Search.withModel, masterDocLoaded_" masterDocLoaded_
              appState = model.appState
              newAppState = { appState |
                 masterDocLoaded = masterDocLoaded_,
                 tool = TableOfContents,
                 page = page }

              updatedModel =
                  { model |
                        appState = newAppState
                      , master_document = defaultMasterDocument
                      , documents2 = model.documents
                  }
          in
              ( { updatedModel | appState = newAppState }
              , Cmd.batch
                  [ Request.Document.getDocumentsWith model.searchState model.current_user.token
                  , RenderAsciidoc.put model.current_document
                  ]
              )

command : String -> SearchOrder -> SearchDomain -> Page -> Model -> Cmd Msg
command query order domain page model =
  let
    newSearchState = SearchState query domain order
    newModel = {model | searchState = newSearchState }
  in
    Request.Document.getDocumentsWith model.searchState model.current_user.token
