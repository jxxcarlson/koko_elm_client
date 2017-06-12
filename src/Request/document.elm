module Request.Document exposing (getDocumentsWith, putCurrentDocument)

import Http exposing (send)
import Json.Decode as Decode exposing (..)
import Request.Api exposing (publicDocumentsUrl, documentsUrl)
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
                publicDocumentsUrl ++ "?all"
            else
                publicDocumentsUrl ++ "?" ++ Action.Search.parseQuery (searchTerms)

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



--|> withExpect (Http.expectJson documentDecoder)


putCurrentDocument : Model -> Cmd Msg
putCurrentDocument model =
    let
        request =
            putDocumentRB model.current_document model.current_user.token
                |> HB.toRequest
    in
        Http.send PutDocument request


decodeDoc : Decoder String
decodeDoc =
    Decode.field "document" Decode.string
