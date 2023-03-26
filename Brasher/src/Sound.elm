module Sound exposing (..)


type Sound
    = Punch
    | Smash


toString : Sound -> String
toString sound =
    case sound of
        Punch ->
            "Punch"

        Smash ->
            "Smash"


sources : List ( String, String )
sources =
    [ ( "punch.mp3", Punch )
    , ( "smash.mp3", Smash )
    ]
        |> List.map (Tuple.mapSecond toString)
