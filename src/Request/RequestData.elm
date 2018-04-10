module Request.RequestData exposing (..)

import Data.Document
-- import Types exposing (DocMsg(..), Document, DocumentDict, Msg(DocMsg))
import Types exposing(..)
import HttpBuilder as HB
import Json.Encode as Encode
import Json.Decode as Decode
import Request.Request exposing (RequestParameters)
import Http

import Data.Document
import Request.Api


type alias DocumentRequestType resourceType = String -> String -> RequestParameters resourceType

--  GetDocuments
-- <function> : Result.Result Http.Error Types.DocumentsRecord -> Types.DocMsg


-- -- getDocumentsParameters : String -> String -> RequestParameters resourceType -> RequestParameters resourceType
-- getDocumentsParameters : String -> String -> (Result.Result Http.Error Types.DocumentsRecord -> Types.DocMsg)-> RequestParameters DocumentsRecord 



-- value type is 
getDocumentsParameters  token tagger route =
    { api = Request.Api.api
    , route = route
    , payload = Encode.null
    , tagger = tagger
    , token = token
    , decoder = Data.Document.decodeDocumentsRecord
    , method = HB.get
    }

}

-- getDocumentsParameters
--     : token
--     -> msg
--     -> route
--     -> { api : String
--     , decoder : Json.Decode.Decoder DocumentsRecord
--     , method : String -> HB.RequestBuilder ()
--     , msg : Result Http.Error resourceType -> Msg
--     , payload : Encode.Value
--     , route : route
--     , token : token
--     }

--     | GetDocuments (Result Http.Error DocumentsRecord)
--     | GetUserDocuments (Result Http.Error DocumentsRecord)
--     | GetSpecialDocument (Result Http.Error DocumentsRecord)
--     | GetMasterDocument (Result Http.Error DocumentsRecord)

--     | CampaignsReceived (Result Http.Error CampaignList)

-- Msg = CampaignsReceived (Result Http.Error CampaignList)
-- getCampaignsData : Model -> RequestParameters CampaignList
-- getCampaignsData model =
--     { api = Configuration.api
--     , route = "/campaigns"
--     , payload = Data.signInCredentialsEncoder model.voter.email model.password
--     , msg = CampaignsReceived
--     , token = ""
--     , decoder = Data.campaignListDecoder
--     , method = HB.get
--     }
