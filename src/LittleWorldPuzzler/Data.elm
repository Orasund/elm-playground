module LittleWorldPuzzler.Data exposing (devMode, gameVersion, maxHistorySize, updateName)


gameVersion : Int
gameVersion =
    5


devMode : Bool
devMode =
    False


updateName : String
updateName =
    "Evergreen"


maxHistorySize : Int
maxHistorySize =
    2 * 100
