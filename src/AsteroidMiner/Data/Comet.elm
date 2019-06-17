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


{-| Conchosprial
<https://en.wikipedia.org/wiki/Conchospiral>
-}
asteroidCoord : Int -> Coord
asteroidCoord cyclesUntilComet =
    let
        t : Float
        t =
            toFloat <| cyclesUntilComet

        maximalCycles : Float
        maximalCycles =
            1

        --16
        maximalRadius : Float
        maximalRadius =
            toFloat <| size // 2 - 1

        --Rotation Speed
        a : Float
        a =
            maximalRadius / (toFloat <| framesPerComet)

        --Slope
        c : Float
        c =
            10

        --opening Angle
        --we need angle = logBase mu (c*framesPerComet)= 2*pi*maximalCycles
        --for M:=maximalCycles, T:=framesPerComet
        --we obtained lg(c*T)/2*pi*M = lg(mu)
        --which can be transformed into:
        --  mu = 2^(lg(c*T)/2*pi*M)
        mu : Float
        mu =
            2 ^ (logBase 2 (c * (toFloat <| framesPerComet)) / (2 * pi * maximalCycles))

        radius : Float
        radius =
            a * t

        angle : Float
        angle =
            logBase mu (c * t)

        ( x, y ) =
            fromPolar ( radius, angle )
    in
    { x = round <| x
    , y = round <| y
    }



{- let
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
-}


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
    if life > 1 then
        { comet
            | life = life - 1
        }

    else
        comet
