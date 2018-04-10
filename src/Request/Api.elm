module Request.Api exposing (..)

import Configuration


host : String
host =
    Configuration.host

api : String 
api = host ++ "/api"

api1 : String
api1 =
    host ++ "/api/"


publicDocumentsUrl : String
publicDocumentsUrl =
    api1 ++ "public/documents"


documentsUrl : String
documentsUrl =
    api1 ++ "documents"


loginUrl : String
loginUrl =
    api1 ++ "authentication"


registerUserUrl : String
registerUserUrl =
    api1 ++ "users"


printUrl : String
printUrl =
    host ++ "/print/documents"

showVersionsUrl : String
showVersionsUrl =
    host ++ "/archive/versions"   


newVersionUrl : String
newVersionUrl =
    host ++ "/archive/new_version"

exportUrl : String
exportUrl =
    host ++ "/export/documents"


imageCatalogueUrl : String
imageCatalogueUrl =
    host ++ "/imagecatalogue/documents"
