module AsteroidMiner.Data exposing (fps, framesPerComet, maxValue, mineVolume, size, spriteSize, version)


fps : Float
fps =
    2


version : String
version =
    "v0.1"


mineVolume : Int
mineVolume =
    128


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
