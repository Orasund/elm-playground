module View.Shop exposing (..)

import Action exposing (Action(..))
import Config
import Html exposing (Html)
import Html.Attributes
import Layout
import Piece exposing (Piece(..))
import Square


toHtml :
    { party : List Piece
    , onCloseOverlay : Action -> msg
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
                                , onPress =
                                    args.party
                                        |> List.indexedMap
                                            (\j ->
                                                if i == j then
                                                    \_ -> newPiece

                                                else
                                                    identity
                                            )
                                        |> NextLevel
                                        |> args.onCloseOverlay
                                        |> Just
                                }
                        )
            )
        |> List.filterMap identity
        |> Layout.column []
    , if List.length args.party < Config.maxPartyMembers then
        Layout.textButton []
            { label = "Recruit " ++ Piece.name Pawn
            , onPress =
                (Pawn :: args.party)
                    |> NextLevel
                    |> args.onCloseOverlay
                    |> Just
            }

      else
        Layout.none
    , Layout.textButton []
        { label = "Leave Shop"
        , onPress =
            args.party
                |> NextLevel
                |> args.onCloseOverlay
                |> Just
        }
    ]
        |> Layout.column [ Layout.gap 8 ]
