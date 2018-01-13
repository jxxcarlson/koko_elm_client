module Nav.Navigation exposing (..)

import Document.Search
import Types exposing (Model, Msg(..), Page(..), SearchDomain(..), SearchOrder(..))


navigateTo : Maybe Page -> Model -> ( Model, Cmd Msg )
navigateTo maybepage model =
    let
        _ =
            Debug.log "MAYBEPAGE" maybepage
    in
    case maybepage of
        Nothing ->
            ( model, Cmd.none )

        Just page ->
            case page of
                PublicPage k ->
                    Document.Search.searchWithParameters ("id:" ++ toString k) Alphabetical Public ReaderPage model

                PrivatePage k ->
                    Document.Search.searchWithParameters ("id:" ++ toString k) Alphabetical Private ReaderPage model

                _ ->
                    ( model, Cmd.none )
