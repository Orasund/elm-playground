module Emojidojo.Data.Config exposing (Config)


type alias Config =
    { jsonstoreId : String
    , roomOpenInMillis : Int
    , nrOfplayers : Int
    , version : Float
    }
