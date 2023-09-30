module View.Shop exposing (..)

import Config
import Html exposing (Html)
import Html.Attributes
import Layout
import Piece exposing (Piece(..))
import Square


toHtml :
    { party : List Piece
    , onLeave : msg
    , onRecruit : Piece -> msg
    , promote : Int -> msg
    }
    -> Html msg
toHtml args =
    [ args.party
        |> List.map
            (\piece ->
                { isWhite = True, piece = piece }
                    |> Square.toString
                    |> Layout.text []
            )
        |> Layout.row [ Html.Attributes.style "font-size" "1.2rem" ]
    , args.party
        |> List.indexedMap
            (\i piece ->
                Piece.promote piece
                    |> Maybe.map
                        (\newPiece ->
                            Layout.textButton []
                                { label = "Promote " ++ Piece.name piece ++ " to " ++ Piece.name newPiece
                                , onPress = args.promote i |> Just
                                }
                        )
            )
        |> List.filterMap identity
        |> Layout.column []
    , if List.length args.party < Config.maxPartyMembers then
        Layout.textButton []
            { label = "Recruit " ++ Piece.name Pawn
            , onPress = args.onRecruit Pawn |> Just
            }

      else
        Layout.none
    , Layout.textButton []
        { label = "Leave Shop"
        , onPress = Just args.onLeave
        }
    ]
        |> Layout.column [ Layout.gap 8 ]
