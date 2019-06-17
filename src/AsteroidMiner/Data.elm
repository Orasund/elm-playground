module AsteroidMiner.Data exposing (fps, framesPerComet, maxValue, size, spriteSize)


fps : Float
fps =
    4



--8


maxValue : Int
maxValue =
    512


framesPerComet : Int
framesPerComet =
    let
        secondsPerComet : Float
        secondsPerComet =
            10 * 60
    in
    round <| secondsPerComet * fps


size : Int
size =
    32


spriteSize : Float
spriteSize =
    8
