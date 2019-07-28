module LittleWorldPuzzler.Data exposing (devMode, gameVersion, maxHistorySize, updateName)


gameVersion : Int
gameVersion =
    5


devMode : Bool
devMode =
    True


updateName : String
updateName =
    "Evergreen"


maxHistorySize : Int
maxHistorySize =
    2 * 100
