module Request.RequestData exposing (..)

import Data.Document
-- import Types exposing (DocMsg(..), Document, DocumentDict, Msg(DocMsg))
import Types exposing(..)
import HttpBuilder as HB
import Json.Encode as Encode
import Request.Request exposing (RequestParameters,  Tagger, SetupRequestData)


import Data.Document
import Request.Api


{-| EXAMPLE OF TAGGER: GetDocuments --
   from Msg = ... GetDocuments (Result Http.Error DocumentsRecord) .. -}
getDocumentsParameters : SetupRequestData DocumentsRecord
getDocumentsParameters  route token tagger =
    { api = Request.Api.api
    , route = route
    , payload = Encode.null
    , tagger = tagger
    , token = token
    , decoder = Data.Document.decodeDocumentsRecord
    , method = HB.get
    }


