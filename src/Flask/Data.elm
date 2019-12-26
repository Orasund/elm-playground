module Flask.Data exposing (baseMult, cardHeight, cardWidth, fontMult, spacingMult)


baseMult : Float
baseMult =
    --5
    5 * 0.38


cardHeight : Int
cardHeight =
    round <| 160 * baseMult


cardWidth : Int
cardWidth =
    round <| 100 * baseMult


spacingMult : Float
spacingMult =
    1 * baseMult


fontMult : Float
fontMult =
    3.6 * baseMult
