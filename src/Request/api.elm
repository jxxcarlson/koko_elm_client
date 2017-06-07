module Request.Api exposing (..)


api : String
api =
    "http://localhost:4000/api/"


getDocumentsUrl =
    api ++ "public/documents"


loginUrl =
    api ++ "authentication"


registerUserUrl =
    api ++ "users"
