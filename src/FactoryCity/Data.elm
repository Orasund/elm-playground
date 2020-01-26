module FactoryCity.Data exposing (devMode, gameVersion, maxBugCycle, maxHistorySize, maxPrice, updateName, yOffset)


yOffset : Int
yOffset =
    50


maxPrice : Int
maxPrice =
    10000


maxBugCycle : Int
maxBugCycle =
    100


gameVersion : Int
gameVersion =
    1


devMode : Bool
devMode =
    True


updateName : String
updateName =
    "Evergreen"


maxHistorySize : Int
maxHistorySize =
    2 * 100
