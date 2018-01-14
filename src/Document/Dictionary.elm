module Document.Dictionary exposing (..)

import Dict
import Platform.Cmd
import Request.Document
import Task
import Types exposing (DocMsg(..), Document, DocumentDict, Msg)


empty : DocumentDict
empty =
    Dict.empty


insert : String -> Document -> DocumentDict -> DocumentDict
insert key document dict =
    Dict.insert key document dict


set : String -> Document -> DocumentDict -> DocumentDict
set key document dict =
    let
        dict2 =
            if member key dict then
                remove key dict
            else
                dict
    in
    insert key document dict2


remove : String -> DocumentDict -> DocumentDict
remove key dict =
    Dict.remove key dict


member : String -> DocumentDict -> Bool
member key dict =
    Dict.member key dict


get : String -> DocumentDict -> Maybe Document
get key dict =
    Dict.get key dict


getContent : String -> DocumentDict -> String
getContent key dict =
    case get key dict of
        Just doc ->
            doc.content

        Nothing ->
            ""


setItemInDict : String -> String -> String -> Cmd Msg
setItemInDict query key token =
    let
        getDocsTask =
            Request.Document.getDocumentsTask "documents" query token

        setItemTask =
            Task.map (\docsRecordResult -> ( docsRecordResult, key )) getDocsTask
    in
    Task.attempt (Types.DocMsg << SetDocumentInDict) setItemTask


setPublicItemInDict : String -> String -> Cmd Msg
setPublicItemInDict query key =
    let
        getDocsTask =
            Request.Document.getPublicDocumentsTask "public/documents" query

        setItemTask =
            Task.map (\docsRecordResult -> ( docsRecordResult, key )) getDocsTask
    in
    Task.attempt (Types.DocMsg << SetDocumentInDict) setItemTask
