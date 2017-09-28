module Document.Dictionary exposing (..)

import Types exposing (Document, DocumentDict, Msg(SetDocumentInDict))
import Dict
import Task
import Request.Document
import Platform.Cmd


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
    case (get key dict) of
        Just doc ->
            doc.content

        Nothing ->
            ""


setItemInDict : String -> String -> String -> Platform.Cmd.Cmd Types.Msg
setItemInDict query key token =
    let
        _ =
            Debug.log "setItemInDict" query

        getDocsTask =
            Request.Document.getDocumentsTask "documents" query token

        setItemTask =
            Task.map (\docsRecordResult -> ( docsRecordResult, key )) getDocsTask
    in
        -- Task.attempt Types.GetDocuments getDocsTask
        Task.attempt SetDocumentInDict setItemTask


setPublicItemInDict : String -> String -> Platform.Cmd.Cmd Types.Msg
setPublicItemInDict query key =
    let
        _ =
            Debug.log "setItemInDict" query

        getDocsTask =
            Request.Document.getPublicDocumentsTask "public/documents" query

        setItemTask =
            Task.map (\docsRecordResult -> ( docsRecordResult, key )) getDocsTask
    in
        -- Task.attempt Types.GetDocuments getDocsTask
        Task.attempt SetDocumentInDict setItemTask



-- Task.attempt Types.GetDocuments (Task.map (\x -> x) getDocsTask)
