module Request.Api exposing (..)

import Configuration


host : String
host =
    Configuration.host


api2 : String
api2 =
    host ++ "/api"


api : String
api =
    host ++ "/api/"


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
