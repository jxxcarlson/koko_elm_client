module Request.Document exposing (getDocumentsWith)

import Http exposing (send)
import Json.Decode as Decode exposing (..)
import Request.Api exposing (getDocumentsUrl)
import Types exposing (..)
import Data.Document exposing (documentDecoder)
import Action.Search exposing (parseQuery)


-- http://package.elm-lang.org/packages/lukewestby/elm-http-extra/5.2.0/Http-Extra


getDocumentsWith : String -> Cmd Msg
getDocumentsWith searchTerms =
    let
        url =
            if searchTerms == "" then
                getDocumentsUrl ++ "?all"
            else
                getDocumentsUrl ++ "?" ++ Action.Search.parseQuery (searchTerms)

        request =
            Http.getString url
    in
        Http.send GetDocuments request



-- saveDocumentRequest : Model -> Http.Request String
-- saveDocumentRequest model =
--     let
--         url =
--             saveDocumentUrl ++ (toString model.selectedDocument.id)
--
--         body =
--             model
--                 |> documentEncoder
--                 |> Http.jsonBody
--     in
--         Http.put saveDocumentUrl body decodeDoc


decodeDoc : Decoder String
decodeDoc =
    Decode.field "document" Decode.string



-- saveDocument : Model -> Cmd Msg
-- saveDocument model =
--     let
--         request =
--             saveDocumentRequest model
--     in
--         Http.send SaveDocument request
--
--
--saveDocument2 : Model -> Http.Request String
--saveDocument2 model =
--    let
--        body =
--            model
--                |> documentEncoder
--                |> Http.jsonBody
--    in
--        Http.post saveDocumentUrl body documentDecoder
