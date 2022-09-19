module Config exposing (..)


viewSize : { width : Float, height : Float }
viewSize =
    { width = 2, height = 3 }


zoom =
    100


width : Int
width =
    viewSize.width * zoom |> round


height : Int
height =
    viewSize.height * zoom |> round
