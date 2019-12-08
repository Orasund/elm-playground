module Emojidojo.String exposing (currentPlayer,data, game, gameId, jsonstore, lastUpdated, openRoom, player, version)


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


game : String
game =
    "/game"


currentPlayer : String
currentPlayer =
    "/currentPlayer"

data : String
data =
    "/data"