module FactoryCity.Data exposing (bugCycle, devMode, gameVersion, maxHistorySize, maxPrice, updateName, yOffset)


yOffset : Int
yOffset =
    50


maxPrice : Int
maxPrice =
    10000


bugCycle : Int
bugCycle =
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
