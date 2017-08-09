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
    let
      searchDomain =  if token == "" then
        Public
      else
        searchState.domain
      _ = Debug.log "Firing search ...., order " searchState.order
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
    Viewed -> "sort=viewed"
    Updated -> "sort=updated"
    Created -> "sort=created"
    Alphabetical -> "sort=title"

buildQuery : List String -> String
buildQuery queryParts =
  "?" ++ String.join "&" queryParts

getPublicDocumentsWith : SearchState -> Cmd Msg
getPublicDocumentsWith searchState =
    let

        query =
            searchState.query

        soq = searchOrderQuery searchState.order
        _ = Debug.log "Firing search with domain = PUBLIC, order" soq

        url =
            if query == "" then
                publicDocumentsUrl ++ buildQuery ["publicdocs=all",  soq]
            else
                publicDocumentsUrl ++ buildQuery [Action.Search.parseQuery (query), soq]

        request =
            Http.getString url
    in
        Http.send GetDocuments request



getUserDocumentsWith : SearchState -> String -> Cmd Msg
getUserDocumentsWith searchState token =
    let
        _ = Debug.log "Firing search with domain = PRIVATE" 1
        query =
            searchState.query
        soq = searchOrderQuery searchState.order

        url =
            if query == "" then
                documentsUrl ++ buildQuery [soq]
            else
              documentsUrl ++ buildQuery [ Action.Search.parseQuery (query), soq]
    in
        HB.get url
            |> HB.withHeader "Authorization" ("Bearer " ++ token)
            |> withExpect (Http.expectJson decodeDocumentsRecord)
            |> HB.send GetUserDocuments

getAllDocumentsWith : SearchState -> String -> Cmd Msg
getAllDocumentsWith searchState token =
    let
        _ = Debug.log "Firing search with domain = ALL" 1
        query =
            searchState.query
        soq = searchOrderQuery searchState.order
        url =
            if query == "" then
                documentsUrl ++ buildQuery ["docs=any", soq]
            else
              documentsUrl ++ buildQuery ["docs=any&" , Action.Search.parseQuery (query), soq]

    in
        HB.get url
            |> HB.withHeader "Authorization" ("Bearer " ++ token)
            |> withExpect (Http.expectJson decodeDocumentsRecord)
            |> HB.send GetUserDocuments

getSpecialDocumentWithQuery : String -> Cmd Msg
getSpecialDocumentWithQuery query =
    let
        url = publicDocumentsUrl ++ "?" ++ query

    in
        HB.get url
            |> withExpect (Http.expectJson decodeDocumentsRecord)
            |> HB.send GetSpecialDocument
        -- Http.send GetSpecialDocument request


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
