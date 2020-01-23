module FactoryCity.Data exposing (bugCycle, devMode, gameVersion, maxHistorySize, maxPrice, updateName)


maxPrice : Int
maxPrice =
    1000


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
