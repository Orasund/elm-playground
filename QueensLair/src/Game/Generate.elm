module Game.Generate exposing (..)

import Game exposing (Game)
import Piece exposing (Piece(..))
import Random exposing (Generator)


generateByLevel : Int -> List Piece -> Generator Game
generateByLevel lv list =
    let
        choose remaining =
            Piece.list
                |> List.filterMap
                    (\piece ->
                        let
                            value =
                                toFloat (Piece.value piece)
                        in
                        if value <= toFloat remaining then
                            Just ( value, piece )

                        else
                            Nothing
                    )
                |> Random.weighted ( 0, Pawn )
    in
    List.range 0 3
        |> List.foldl
            (\_ ->
                Random.andThen
                    (\args ->
                        if args.remaining == 0 then
                            Random.constant args

                        else
                            choose args.remaining
                                |> Random.map
                                    (\piece ->
                                        { remaining = args.remaining - Piece.value piece
                                        , black = piece :: args.black
                                        }
                                    )
                    )
            )
            (Random.constant { remaining = 2 * lv, black = [] })
        |> Random.map
            (\{ black } ->
                Game.fromPieces
                    { white = list
                    , black = black
                    }
            )
