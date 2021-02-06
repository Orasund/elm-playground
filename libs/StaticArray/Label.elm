module StaticArray.Label exposing (Eight, EightPlus, Five, FivePlus, Four, FourPlus, Nine, One(..), OnePlus(..), Seven, Ten, TenPlus, Three, Twenty, TwentyPlus, Two, TwoPlus)


type OnePlus a
    = OnePlus a


type alias TwoPlus a =
    OnePlus (OnePlus a)


type alias FourPlus a =
    TwoPlus (TwoPlus a)


type alias EightPlus a =
    FourPlus (FourPlus a)


type alias FivePlus a =
    OnePlus (FourPlus a)


type alias TenPlus a =
    FivePlus (FivePlus a)


type alias TwentyPlus a =
    TenPlus (TenPlus a)


type One
    = One


type alias Two =
    OnePlus One


type alias Three =
    OnePlus Two


type alias Four =
    OnePlus Three


type alias Five =
    OnePlus Four


type alias Six =
    OnePlus Five


type alias Seven =
    OnePlus Six


type alias Eight =
    OnePlus Seven


type alias Nine =
    OnePlus Eight


type alias Ten =
    OnePlus Nine


type alias Twenty =
    TenPlus Ten
