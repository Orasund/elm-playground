module Emojidojo.String exposing (jsonstore, lastUpdated, openRoom,gameId, player, version)


jsonstore : String
jsonstore =
    "https://www.jsonstore.io/"


openRoom : String
openRoom =
    "/openRoom"


lastUpdated : String
lastUpdated =
    "/lastUpdated"


player : String
player =
    "/player"


version : String
version =
    "/version"


gameId : String
gameId =
    "/gameId"
