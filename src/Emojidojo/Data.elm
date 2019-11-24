module Emojidojo.Data exposing (nrOfplayers, roomOpenInMillis, url, version)

import Emojidojo.Config exposing (id)
import Emojidojo.String as String


roomOpenInMillis : Int
roomOpenInMillis =
    1000 * 10


nrOfplayers : Int
nrOfplayers =
    2


url : String
url =
    String.jsonstore ++ id


version : Float
version =
    0.1001
