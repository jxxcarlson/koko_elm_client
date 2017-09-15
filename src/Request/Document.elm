module Request.Document
    exposing
        ( getDocumentsWith
        , getSpecialDocumentWithQuery
        , put
        , createDocument
        , deleteCurrentDocument
        , getDocumentsTask
        , getSpecialDocumentWithAuthenticatedQuery
        , reloadMasterDocument
        , saveDocumentTask
        )

import Http exposing (send)
import Json.Decode as Decode exposing (..)
import Request.Api exposing (publicDocumentsUrl, documentsUrl, api)
import Types exposing (..)
import Data.Document
    exposing
        ( documentDecoder
        , documentsDecoder
        , documentRecordDecoder
        , decodeDocumentsRecord
        )
import Document.QueryParser exposing (parseQuery)
import HttpBuilder as HB exposing (..)
import Task


-- http://package.elm-lang.org/packages/lukewestby/elm-http-extra/5.2.0/Http-Extra


getDocumentsWith : SearchState -> String -> Cmd Msg
getDocumentsWith searchState token =
    let
        _ =
            Debug.log "IN getDocumentsWith, searchState is" searchState

        searchDomain =
            if token == "" then
                Public
            else
                searchState.domain

        ( message, route ) =
            messageRoute searchDomain

        _ =
            Debug.log "Firing search ...., order " searchState.order
    in
        getDocuments route (makeQuery searchState) message token


makeQuery searchState =
    let
        basicQuery =
            if searchState.query == "" then
                "publicdocs=all"
            else
                parseQuery (searchState.query)

        soq =
            searchOrderQuery searchState.order

        prefix =
            case ( searchState.domain, searchState.query ) of
                ( _, "" ) ->
                    "publicdocs=all"

                ( All, _ ) ->
                    "docs=any"

                ( _, _ ) ->
                    ""

        queryList =
            [ prefix ] ++ [ parseQuery (searchState.query), soq ]
    in
        buildQuery queryList


messageRoute searchDomain =
    case searchDomain of
        Public ->
            ( GetDocuments, "public/documents" )

        Private ->
            ( GetUserDocuments, "documents" )

        All ->
            ( GetUserDocuments, "documents" )


searchOrderQuery : SearchOrder -> String
searchOrderQuery searchOrder =
    case searchOrder of
        Viewed ->
            "sort=viewed"

        Updated ->
            "sort=updated"

        Created ->
            "sort=created"

        Alphabetical ->
            "sort=title"


buildQuery : List String -> String
buildQuery queryParts =
    queryParts
        |> List.filter (\x -> x /= "")
        |> String.join "&"



-- (Result Http.Error DocumentsRecord)
-- (Result Http.Error DocumentsRecord -> msg)
-- getDocuments : String -> String -> Result Http.Error DocumentsRecord -> String -> Cmd msg


getDocuments : String -> String -> (Result Http.Error DocumentsRecord -> Msg) -> String -> Cmd Msg
getDocuments route query message token =
    let
        url =
            api ++ route ++ "?" ++ parseQuery (query)
    in
        HB.get url
            |> HB.withHeader "Authorization" ("Bearer " ++ token)
            |> withExpect (Http.expectJson decodeDocumentsRecord)
            |> HB.send message


getDocumentsTask : String -> String -> String -> Task.Task Http.Error DocumentsRecord
getDocumentsTask route query token =
    let
        url =
            api ++ route ++ "?" ++ parseQuery (query)

        request =
            HB.get url
                |> HB.withHeader "Authorization" ("Bearer " ++ token)
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
    in
        request |> Http.toTask


getSpecialDocumentWithQuery : String -> Cmd Msg
getSpecialDocumentWithQuery query =
    getDocuments "public/documents" query GetSpecialDocument ""


getSpecialDocumentWithAuthenticatedQuery : String -> String -> Cmd Msg
getSpecialDocumentWithAuthenticatedQuery token query =
    let
        url =
            documentsUrl ++ "?" ++ query
    in
        HB.get url
            |> HB.withHeader "Authorization" ("Bearer " ++ token)
            |> withExpect (Http.expectJson decodeDocumentsRecord)
            |> HB.send GetSpecialDocument



-- Http.send GetSpecialDocument request


reloadMasterDocument : Int -> String -> Cmd Msg
reloadMasterDocument doc_id token =
    let
        url =
            documentsUrl ++ "?" ++ "id=" ++ (toString doc_id)
    in
        HB.get url
            |> HB.withHeader "Authorization" ("Bearer " ++ token)
            |> withExpect (Http.expectJson decodeDocumentsRecord)
            |> HB.send GetMasterDocument


deleteCurrentDocumentRB : Model -> RequestBuilder ()
deleteCurrentDocumentRB model =
    let
        url =
            documentsUrl ++ "/" ++ (toString model.current_document.id)

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
        Http.send DeleteDocument request



-- http://package.elm-lang.org/packages/lukewestby/elm-http-builder/latest/HttpBuilder


putDocumentRB : String -> String -> Document -> RequestBuilder ()
putDocumentRB queryString token document =
    let
        params =
            Data.Document.documentEncoder document

        url =
            if queryString == "" then
                documentsUrl ++ "/" ++ (toString document.id)
            else
                documentsUrl ++ "/" ++ (toString document.id) ++ "?" ++ queryString
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
        Http.send PutDocument request


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


decodeDoc : Decoder String
decodeDoc =
    Decode.field "document" Decode.string
