module Request.RequestData exposing (..)

import Data.Document
import Types
    exposing
        ( Model
        
        )
import HttpBuilder as HB
import Json.Encode as Encode
import Configuration
import Request.Request exposing (RequestParameters)



-- signupParameters : Model -> RequestParameters VerifiedVoter
-- signupParameters model =
--     { api = Configuration.api
--     , route = "/voters"
--     , payload = Data.encodeVoter model
--     , resourceType = CreateVoter
--     , token = ""
--     , decoder = Data.verifiedVoterDecoder
--     , method = HB.post
--     }


-- inviteBuddyRequestData : Model -> RequestParameters Int
-- inviteBuddyRequestData model =
--     let
--         buddy_ =
--             model.buddy

--         buddy =
--             { buddy_ | campaign = model.voter.campaign }
--     in
--         { api = Configuration.api
--         , route = "/voters/invitebuddy"
--         , payload = Data.buddyEncoder buddy
--         , resourceType = InviteBuddy
--         , token = model.token
--         , decoder = Data.pointsDecoder
--         , method = HB.post
--         }

