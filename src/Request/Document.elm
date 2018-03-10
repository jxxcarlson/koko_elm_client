module Request.Document
    exposing
        ( createDocument
        , deleteCurrentDocument
        , getDocumentWithAuthenticatedQuery
        , getDocumentWithAuthenticatedQueryTask
        , getDocumentWithId
        , getDocumentWithQuery
        , getDocuments
        , getDocumentsRequest
        , getDocumentsTask
        , getPublicDocumentsTask
        , put
        , reloadMasterDocument
        , saveDocumentTask
        )

import Data.Document
    exposing
        ( decodeDocumentsRecord
        , documentDecoder
        , documentRecordDecoder
        , documentsDecoder
        )
import Document.QueryParser exposing (parseQuery)
import Http exposing (send)
import HttpBuilder as HB exposing (..)
import Json.Decode as Decode exposing (..)
import Request.Api exposing (api, documentsUrl, publicDocumentsUrl)
import Task
import Types exposing (..)


-- http://package.elm-lang.org/packages/lukewestby/elm-http-extra/5.2.0/Http-Extra
-- getDocumentsRequest : String -> String -> (Result Http.Error DocumentsRecord -> Msg) -> String -> Cmd Msg


getDocumentsRequest : String -> String -> (Result Http.Error DocumentsRecord -> Msg) -> String -> HB.RequestBuilder Types.DocumentsRecord
getDocumentsRequest route query message token =
    let
        url = Debug.log "Request.getDocumentsRequest"
            (api ++ route ++ "?" ++ parseQuery query)
    in
    HB.get url
        |> HB.withHeader "Authorization" ("Bearer " ++ token)
        |> withExpect (Http.expectJson decodeDocumentsRecord)


getDocuments : String -> String -> (Result Http.Error DocumentsRecord -> Msg) -> String -> Cmd Msg
getDocuments route query message token =
    getDocumentsRequest route query message token
        |> HB.send message


getDocumentWithId : String -> (Result Http.Error DocumentsRecord -> Msg) -> String -> Int -> Cmd Msg
getDocumentWithId route message token id =
    let
        query =  Debug.log "Request.getDocumentWithId, id"
            ("id=" ++ toString id)
    in
    getDocumentsRequest route query message token
        |> HB.send message


getDocumentsTask : String -> String -> String -> Task.Task Http.Error DocumentsRecord
getDocumentsTask route query token =
    let
        url =
            api ++ route ++ "?" ++ parseQuery query

        request =
            HB.get url
                |> HB.withHeader "Authorization" ("Bearer " ++ token)
                |> withExpect (Http.expectJson decodeDocumentsRecord)
                |> HB.toRequest
    in
    request |> Http.toTask


getPublicDocumentsTask : String -> String -> Task.Task Http.Error DocumentsRecord
getPublicDocumentsTask route query =
    let
        url =
            api ++ route ++ "?" ++ parseQuery query

        request =
            HB.get url
                |> withExpect (Http.expectJson decodeDocumentsRecord)
                |> HB.toRequest
    in
    request |> Http.toTask



-- saveDocumentCmd : String -> Document -> Model -> Cmd Msg


saveDocumentTask : String -> Document -> Model -> Task.Task Http.Error ()
saveDocumentTask queryString document model =
    let
        request =
            putDocumentRB queryString model.current_user.token document
                |> HB.toRequest

        _ =
            Debug.log "saveDocumentTask with queryString" queryString
    in
    request |> Http.toTask


getDocumentWithQuery : (Result.Result Http.Error DocumentsRecord -> Msg) -> String -> Cmd Msg
getDocumentWithQuery processor query =
    getDocuments "public/documents" query processor ""


getDocumentWithAuthenticatedQuery : (Result.Result Http.Error DocumentsRecord -> Msg) -> String -> String -> Cmd Msg
getDocumentWithAuthenticatedQuery processor token query =
    let
        url =
            documentsUrl ++ "?" ++ query
    in
    HB.get url
        |> HB.withHeader "Authorization" ("Bearer " ++ token)
        |> withExpect (Http.expectJson decodeDocumentsRecord)
        |> HB.send processor


getDocumentWithAuthenticatedQueryTask : String -> String -> Task.Task Http.Error DocumentsRecord
getDocumentWithAuthenticatedQueryTask token query =
    let
        url =
            Debug.log "getDocumentWithAuthenticatedQueryTask, route & query"
                (documentsUrl ++ "?" ++ query)
    in
    HB.get url
        |> HB.withHeader "Authorization" ("Bearer " ++ token)
        |> withExpect (Http.expectJson decodeDocumentsRecord)
        |> HB.toRequest
        |> Http.toTask


getSpecialDocumentWithAuthenticatedQuery : String -> String -> Cmd Msg
getSpecialDocumentWithAuthenticatedQuery token query =
    let
        url =
            documentsUrl ++ "?" ++ query
    in
    HB.get url
        |> HB.withHeader "Authorization" ("Bearer " ++ token)
        |> withExpect (Http.expectJson decodeDocumentsRecord)
        |> HB.send (DocMsg << GetSpecialDocument)



-- getDocumentForDictionaryWithToken : String -> String -> String -> Cmd Msg
-- getDocumentForDictionaryWithToken key token query =
--     let
--         url =
--             documentsUrl ++ "?" ++ query
--     in
--         HB.get url
--             |> HB.withHeader "Authorization" ("Bearer " ++ token)
--             |> withExpect (Http.expectJson decodeDocumentsRecord)
--             |> HB.send GetDocumentForDictionary
-- Http.send GetSpecialDocument request


reloadMasterDocument : Int -> String -> Cmd Msg
reloadMasterDocument doc_id token =
    let
        url =
            documentsUrl ++ "?" ++ "id=" ++ toString doc_id
    in
    HB.get url
        |> HB.withHeader "Authorization" ("Bearer " ++ token)
        |> withExpect (Http.expectJson decodeDocumentsRecord)
        |> HB.send (DocMsg << GetMasterDocument)


deleteCurrentDocumentRB : Model -> RequestBuilder ()
deleteCurrentDocumentRB model =
    let
        url =
            documentsUrl ++ "/" ++ toString model.current_document.id

        token =
            model.current_user.token
    in
    HB.delete url
        |> HB.withHeader "Authorization" ("Bearer " ++ token)


deleteCurrentDocument : Model -> Cmd Msg
deleteCurrentDocument model =
    let
        request =
            deleteCurrentDocumentRB model
                |> HB.toRequest
    in
    Http.send (DocMsg << DeleteDocument) request



-- http://package.elm-lang.org/packages/lukewestby/elm-http-builder/latest/HttpBuilder


putDocumentRB : String -> String -> Document -> RequestBuilder ()
putDocumentRB queryString token document =
    let
        params =
            Data.Document.documentEncoder document

        url =
            if queryString == "" then
                documentsUrl ++ "/" ++ toString document.id
            else
                documentsUrl ++ "/" ++ toString document.id ++ "?" ++ queryString
    in
    HB.put url
        |> HB.withHeader "Authorization" ("Bearer " ++ token)
        |> withJsonBody params


put : String -> Model -> Document -> Cmd Msg
put queryString model document =
    let
        request =
            putDocumentRB queryString model.current_user.token document
                |> HB.toRequest
    in
    Http.send (DocMsg << PutDocument) request



-- createDocument : Document -> String -> Cmd Msg


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
        |> HB.send (DocMsg << CreateDocument)


decodeDoc : Decoder String
decodeDoc =
    Decode.field "document" Decode.string
