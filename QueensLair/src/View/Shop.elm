module View.Shop exposing (..)

import Html exposing (Html)
import Layout
import Piece exposing (Piece)


toHtml :
    { points : Int
    , onLeave : msg
    , onRecruit : Piece -> msg
    }
    -> Html msg
toHtml args =
    [ "Points:" ++ String.fromInt args.points |> Layout.text []
    , Piece.list
        |> List.filterMap
            (\piece ->
                if Piece.value piece <= args.points then
                    Layout.textButton []
                        { label = "Recruit " ++ Piece.name piece
                        , onPress = args.onRecruit piece |> Just
                        }
                        |> Just

                else
                    Nothing
            )
        |> Layout.column []
    , Layout.textButton []
        { label = "Leave Shop"
        , onPress = Just args.onLeave
        }
    ]
        |> Layout.column [ Layout.gap 8 ]
