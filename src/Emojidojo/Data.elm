module Emojidojo.Data exposing (roomOpenInMillis, url)

import Emojidojo.Config exposing (id)
import Emojidojo.String as String


roomOpenInMillis : Int
roomOpenInMillis =
    1000 * 10


url : String
url =
    String.jsonstore ++ id
