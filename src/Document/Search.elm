module Document.Search exposing(withParameters, withModel)

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

              appState = model.appState
              newAppState = { appState |
                 masterDocLoaded = False,
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
