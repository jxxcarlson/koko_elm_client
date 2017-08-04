module Request.Document exposing (getDocumentsWith, getSpecialDocumentWithQuery, putDocument,
  createDocument, deleteCurrentDocument)

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

getSpecialDocumentWithQuery : String -> Cmd Msg
getSpecialDocumentWithQuery query =
    let
        url = publicDocumentsUrl ++ "?" ++ query

    in
        HB.get url
            |> withExpect (Http.expectJson decodeDocumentsRecord)
            |> HB.send GetSpecialDocument
        -- Http.send GetSpecialDocument request



getUserDocumentsWith : SearchState -> String -> Cmd Msg
getUserDocumentsWith searchState token =
    let
        query =
            searchState.query

        url =
            if query == "" then
                documentsUrl ++ "?all"
            else
                documentsUrl ++ "?" ++ Action.Search.parseQuery (query)
    in
        HB.get url
            |> HB.withHeader "Authorization" ("Bearer " ++ token)
            |> withExpect (Http.expectJson decodeDocumentsRecord)
            |> HB.send GetUserDocuments


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


putDocumentRB : String -> String -> Document ->  RequestBuilder ()
putDocumentRB queryString token document =
    let
        params =
            Data.Document.documentEncoder document

        url = if queryString == "" then
                documentsUrl ++ "/" ++ (toString document.id)
              else
                documentsUrl ++ "/" ++ (toString document.id) ++ "?" ++ queryString
    in
        HB.put url
            |> HB.withHeader "Authorization" ("Bearer " ++ token)
            |> withJsonBody params


putDocument : String -> Model -> Document -> Cmd Msg
putDocument queryString model document =
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
