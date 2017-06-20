module Views2.Reader exposing (..)

import Style exposing (..)
import StyleSheet exposing (..)
import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events exposing (..)
import Style exposing (..)
import Types exposing (..)
import Action.UI exposing (appStateWithPage)
import Views2.Common as Common
{-

-}

reeder model =
  column None
    []
    [
    Common.toolSelectorPanel model
    , Common.documentListView model
  ]
