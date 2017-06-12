module Request.Api exposing (..)


api : String
api =
    "http://localhost:4000/api/"


publicDocumentsUrl =
    api ++ "public/documents"


documentsUrl =
    api ++ "documents"


loginUrl =
    api ++ "authentication"


registerUserUrl =
    api ++ "users"
