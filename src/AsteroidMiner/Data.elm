module AsteroidMiner.Data exposing (fps, framesPerComet, size)


fps : Float
fps =
    1 / 4


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
