module Flask.Data.Element exposing (Element(..), order, toString)


type Element
    = Red
    | Blue
    | Yellow
    | Green
    | Any


order : List Element -> List Element
order list =
    let
        { red, blue, yellow, green, any } =
            list
                |> List.foldl
                    (\elem out ->
                        case elem of
                            Red ->
                                { out | red = out.red + 1 }

                            Blue ->
                                { out | blue = out.blue + 1 }

                            Yellow ->
                                { out | yellow = out.yellow + 1 }

                            Green ->
                                { out | green = out.green + 1 }

                            Any ->
                                { out | any = out.any + 1 }
                    )
                    { red = 0, blue = 0, yellow = 0, green = 0, any = 0 }
    in
    List.concat
        [ Green |> List.repeat green
        , Yellow |> List.repeat yellow
        , Blue |> List.repeat blue
        , Red |> List.repeat red
        , Any |> List.repeat any
        ]


toString : Element -> String
toString element =
    case element of
        Red ->
            "💥"

        Blue ->
            "📘"

        Yellow ->
            "💰"

        Green ->
            "💚"

        Any ->
            "❔"
