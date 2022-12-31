module View.Animation exposing (..)

import Array exposing (Array)
import Data.Animation exposing (Animation)
import Data.Floor
import Data.World exposing (World)
import Html exposing (Html)
import View.Screen


emptyWorld : { width : Int, height : Int } -> World
emptyWorld args =
    List.range 0 (args.height - 1)
        |> List.concatMap
            (\y ->
                List.range 0 (args.width - 1)
                    |> List.map (\x -> ( x, y ))
            )
        |> List.foldl (Data.World.insertFloor Data.Floor.Ground)
            Data.World.empty


animate : Float -> Animation -> Int -> Html msg
animate widthOverHeight animation =
    animation.frames
        |> Array.map
            (View.Screen.animation
                { width = animation.width
                , height = animation.height
                , widthOverHeight = widthOverHeight
                }
            )
        |> fromArray


fromArray : Array (Html msg) -> Int -> Html msg
fromArray array i =
    array
        |> Array.get (modBy (Array.length array) i)
        |> Maybe.withDefault (Html.text "")
