module Request.Api exposing (..)

import Configuration

host : String
host = Configuration.host
-- host = "https://mysterious-forest-36511.herokuapp.com"

api : String
api = host ++ "/api/"

publicDocumentsUrl : String
publicDocumentsUrl =
    api ++ "public/documents"

documentsUrl : String
documentsUrl =
    api ++ "documents"

loginUrl : String
loginUrl =
    api ++ "authentication"

registerUserUrl : String
registerUserUrl =
    api ++ "users"

printUrl : String
printUrl = host ++ "/print/documents"
