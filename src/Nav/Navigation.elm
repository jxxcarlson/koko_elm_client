module Nav.Navigation exposing(..)

import Types exposing(Page(..), Model, SearchDomain(..))
import Action.Document

navigateTo maybepage model =
  case maybepage of
      Nothing ->
            (model, Cmd.none)

      Just page ->
        case page of
          PublicPage k ->
            Action.Document.search Public ("id:" ++ (toString k)) ReaderPage model
          PrivatePage k ->
            Action.Document.search Private ("id:" ++ (toString k)) ReaderPage model
          _ ->
            ( { model | message = "MESSAGE: something else"  }, Cmd.none )
