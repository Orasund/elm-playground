module AsteroidMiner.Data exposing (floorCosts, fps, framesPerComet, maxValue, mineVolume, size, spriteSize, version)


fps : Float
fps =
    2


version : String
version =
    "v0.4"


floorCosts : Int
floorCosts =
    512


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
            5 * 60
    in
    round <| secondsPerComet * fps


size : Int
size =
    32


spriteSize : Float
spriteSize =
    8
