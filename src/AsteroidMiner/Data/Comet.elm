module AsteroidMiner.Data.Comet exposing (Comet, new, position, update)

import AsteroidMiner.Data exposing (framesPerComet, size)
import Grid.Position as Position exposing (Coord, Position)
import Location exposing (Angle(..))


type alias Comet =
    { life : Int
    , offset : Angle
    }


new : Angle -> Comet
new angle =
    { life = framesPerComet
    , offset = angle
    }


asteroidCoord : Int -> Coord
asteroidCoord cyclesUntilComet =
    let
        scale : Float
        scale =
            (toFloat <| size // 2 - 1)
                * logBase
                    (toFloat <| framesPerComet)
                    (toFloat <| cyclesUntilComet)

        maximalCycles : Float
        maximalCycles =
            16

        { x, y } =
            Location.fromAngle
                (Angle <|
                    (*) (2 * pi) <|
                        (maximalCycles
                            * (toFloat <| (framesPerComet - cyclesUntilComet) ^ 2)
                        )
                            / (toFloat <| framesPerComet ^ 2)
                )
                |> Location.scaleBy scale
    in
    { x = round <| x
    , y = round <| y
    }


position : Comet -> Position
position { life } =
    let
        center : Int
        center =
            size // 2
    in
    ( center, center )
        |> Position.add (asteroidCoord life)


update : Comet -> Comet
update ({ life } as comet) =
    if life > 0 then
        { comet
            | life = life - 1
        }

    else
        comet
