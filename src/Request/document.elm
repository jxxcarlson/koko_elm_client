module Request.Document exposing (getDocumentsWith)

import Http exposing (send)
import Json.Decode as Decode exposing (..)
import Request.Api exposing (documentsUrl)
import Types exposing (..)
import Data.Document exposing (documentDecoder)
import Action.Search exposing (parseQuery)
import HttpBuilder as HB exposing (..)


-- http://package.elm-lang.org/packages/lukewestby/elm-http-extra/5.2.0/Http-Extra


getDocumentsWith : String -> Cmd Msg
getDocumentsWith searchTerms =
    let
        url =
            if searchTerms == "" then
                documentsUrl ++ "?all"
            else
                documentsUrl ++ "?" ++ Action.Search.parseQuery (searchTerms)

        request =
            Http.getString url
    in
        Http.send GetDocuments request



-- http://package.elm-lang.org/packages/lukewestby/elm-http-builder/latest/HttpBuilder


putDocumentRB : Document -> String -> RequestBuilder ()
putDocumentRB document token =
    let
        params =
            Data.Document.documentEncoder document

        url =
            documentsUrl ++ "/" ++ (toString document.id)
    in
        HB.put url
            |> HB.withHeader "Authorization" ("Bearer " ++ token)
            |> withJsonBody params



-- putCurrentDocument : Model -> Cmd Msg
-- putCurrentDocument model =
--     let
--         request =
--             putDocumentRB model.current_document model.current_user.token
--                 |> HB.toRequest
--     in
-- Http.send PutDocument request
--
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
