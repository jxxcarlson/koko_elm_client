module Request.Document
    exposing
        ( getDocumentsWith
        , getSpecialDocumentWithQuery
        , put
        , createDocument
        , deleteCurrentDocument
        , getSpecialDocumentWithAuthenticatedQuery
        , reloadMasterDocument
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

        _ =
            Debug.log "Firing search ...., order " searchState.order
    in
        case searchDomain of
            Public ->
                getPublicDocumentsWith searchState

            Private ->
                getUserDocumentsWith searchState token

            All ->
                getAllDocumentsWith searchState token


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
    String.join "&" queryParts


getPublicDocumentsWith : SearchState -> Cmd Msg
getPublicDocumentsWith searchState =
    let
        _ =
            Debug.log "Firing search with domain = Public" "now"

        soq =
            searchOrderQuery searchState.order

        basicQuery =
            if searchState.query == "" then
                "publicdocs=all"
            else
                parseQuery (searchState.query)

        query =
            buildQuery [ basicQuery, soq ]
    in
        getDocuments "public/documents" query GetDocuments ""


getUserDocumentsWith : SearchState -> String -> Cmd Msg
getUserDocumentsWith searchState token =
    let
        _ =
            Debug.log "Firing search with domain = Private" "now"

        soq =
            searchOrderQuery searchState.order

        query =
            buildQuery [ parseQuery (searchState.query), soq ]
    in
        getDocuments "documents" query GetUserDocuments token


getAllDocumentsWith : SearchState -> String -> Cmd Msg
getAllDocumentsWith searchState token =
    let
        _ =
            Debug.log "Firing search with domain = ALL" "now"

        soq =
            searchOrderQuery searchState.order

        query =
            buildQuery [ "docs=any", parseQuery (searchState.query), soq ]
    in
        getDocuments "documents" query GetUserDocuments token



-- getDocuments : String -> (Result Http.Error String) -> Cmd Msg


getDocuments route query processor token =
    let
        url =
            api ++ route ++ "?" ++ parseQuery (query)
    in
        HB.get url
            |> HB.withHeader "Authorization" ("Bearer " ++ token)
            |> withExpect (Http.expectJson decodeDocumentsRecord)
            |> HB.send processor


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
