module Sprawlopolis.Rule exposing (view)


view : Int -> ( List String, List String )
view int =
    case int of
        1 ->
            ( [ "Park blocks"
              , "adjacent to"
              , "this block"
              , "give +1"
              , "point"
              ]
            , [ "You get"
              , "-1 points"
              ]
            )

        2 ->
            ( [ "Only the"
              , "biggest park"
              , "areas give "
              , "points"
              ]
            , []
            )

        3 ->
            ( [ "Park blocks"
              , "give 1 point"
              ]
            , [ "Industrial"
              , "blocks give"
              , "-3 points"
              ]
            )

        4 ->
            ( [ "Every park"
              , "area gives"
              , "1 point"
              ]
            , [ "Every street"
              , "gives"
              , "-1 point"
              ]
            )

        5 ->
            ( [ "Commertial"
              , "blocks adjacent"
              , "to this block"
              , "give +1"
              , "point"
              ]
            , [ "You get"
              , "-1 points"
              ]
            )

        6 ->
            ( [ "Park blocks"
              , "adjacent to"
              , "residential"
              , "blocks give"
              , "+1 point"
              ]
            , [ "Industrial"
              , "blocks adjacent"
              , "to residential"
              , "blocks give"
              , "-1 point"
              ]
            )

        7 ->
            ( [ "Only the"
              , "biggest"
              , "commertial"
              , "areas"
              , "give points"
              ]
            , []
            )

        8 ->
            ( [ "Industrial"
              , "blocks adjacent"
              , "to this block"
              , "give +1"
              , "point"
              ]
            , [ "You get"
              , "-1 points"
              ]
            )

        9 ->
            ( [ "Only the"
              , "biggest"
              , "industrial"
              , "areas"
              , "give points"
              ]
            , []
            )

        10 ->
            ( [ "+1 Point for"
              , "every street that"
              , "ends at the"
              , "edge of"
              , "the city"
              ]
            , [ "-1 Point"
              , "for every"
              , "street that"
              , "ends with in"
              , "the city"
              ]
            )

        11 ->
            ( [ "Residential"
              , "blocks adjacent"
              , "to this block"
              , "give +1"
              , "point"
              ]
            , [ "You get"
              , "-1 points"
              ]
            )

        12 ->
            ( [ "Only the"
              , "biggest"
              , "residential"
              , "areas"
              , "give points"
              ]
            , []
            )

        13 ->
            ( [ "Residential"
              , "block"
              , "give +1 point"
              ]
            , [ "Commertial"
              , "blocks give"
              , "-2 points"
              ]
            )

        14 ->
            ( [ "Residential"
              , "areas"
              , "connected to a"
              , "commertial area"
              , "give +1 point"
              ]
            , []
            )

        15 ->
            ( [ "Residential"
              , "areas"
              , "connected to a"
              , "commertial area"
              , "give +1 point"
              ]
            , []
            )

        16 ->
            ( [ "Commertial"
              , "areas that are"
              , "surrounded"
              , "by blocks get"
              , "+2 point"
              ]
            , [ "Commertial"
              , "areas on the"
              , "edge of the"
              , "city give"
              , "-1 point"
              ]
            )

        _ ->
            ( [ "Rule " ++ String.fromInt int ], [] )
