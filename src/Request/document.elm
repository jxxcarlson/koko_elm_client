module Request.Document exposing (getDocumentsWith, putCurrentDocument, createDocument)

import Http exposing (send)
import Json.Decode as Decode exposing (..)
import Request.Api exposing (publicDocumentsUrl, documentsUrl)
import Types exposing (..)
import Data.Document
    exposing
        ( documentDecoder
        , documentsDecoder
        , documentRecordDecoder
        , decodeDocumentsRecord
        )
import Action.Search exposing (parseQuery)
import HttpBuilder as HB exposing (..)


-- http://package.elm-lang.org/packages/lukewestby/elm-http-extra/5.2.0/Http-Extra


getDocumentsWith : SearchState -> String -> Cmd Msg
getDocumentsWith searchState token =
    if searchState.domain == Private && token /= "" then
        getUserDocumentsWith searchState token
    else
        getPublicDocumentsWith searchState


getPublicDocumentsWith : SearchState -> Cmd Msg
getPublicDocumentsWith searchState =
    let
        query =
            searchState.query

        url =
            if query == "" then
                publicDocumentsUrl ++ "?all"
            else
                publicDocumentsUrl ++ "?" ++ Action.Search.parseQuery (query)

        request =
            Http.getString url
    in
        Http.send GetDocuments request


getUserDocumentsWith : SearchState -> String -> Cmd Msg
getUserDocumentsWith searchState token =
    let
        url =
            documentsUrl
    in
        HB.get url
            |> HB.withHeader "Authorization" ("Bearer " ++ token)
            |> withExpect (Http.expectJson decodeDocumentsRecord)
            |> HB.send GetUserDocuments



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



--|> withExpect (Http.expectJson documentDecoder)


putCurrentDocument : Model -> Cmd Msg
putCurrentDocument model =
    let
        request =
            putDocumentRB model.current_document model.current_user.token
                |> HB.toRequest
    in
        Http.send PutDocument request



--- aaa
-- Create New Document


createDocument : Document -> String -> Cmd Msg
createDocument document token =
    let
        params =
            Data.Document.documentEncoder document

        url =
            documentsUrl
    in
        HB.post url
            |> HB.withHeader "Authorization" ("Bearer " ++ token)
            |> withJsonBody params
            |> withExpect (Http.expectJson documentRecordDecoder)
            |> HB.send CreateDocument



-- createDocument : Model -> Cmd Msg
-- createDocument model =
--     let
--         request =
--             createDocumentRB model.current_document model.current_user.token
--                 |> HB.toRequest
--     in
--         HB.send CreateDocument request


decodeDoc : Decoder String
decodeDoc =
    Decode.field "document" Decode.string
