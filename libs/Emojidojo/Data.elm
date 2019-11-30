module Emojidojo.Data exposing (url)

import Emojidojo.Data.Config exposing (Config)
import Emojidojo.String as String


url : Config -> String
url { jsonstoreId } =
    String.jsonstore ++ jsonstoreId
