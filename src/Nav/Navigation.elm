module Nav.Navigation exposing (..)

import Types exposing (Page(..), Model, SearchDomain(..), Msg(..), SearchOrder(..))
import Document.Search


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
                        Document.Search.withParameters ("id:" ++ (toString k)) Alphabetical Public ReaderPage model

                    PrivatePage k ->
                        Document.Search.withParameters ("id:" ++ (toString k)) Alphabetical Private ReaderPage model

                    _ ->
                        ( model, Cmd.none )
