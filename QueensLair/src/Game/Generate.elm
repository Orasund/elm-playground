module Game.Generate exposing (..)

import Level exposing (Level)
import Piece exposing (Piece(..))
import Random exposing (Generator)


generateByLevel : Int -> List Piece -> Generator Level
generateByLevel lv list =
    let
        choose remaining black =
            Piece.list
                |> List.filterMap
                    (\piece ->
                        let
                            amount =
                                black
                                    |> List.filter ((==) piece)
                                    |> List.length

                            value =
                                toFloat (Piece.value piece)
                        in
                        if
                            value
                                <= toFloat remaining
                                && (piece == Pawn || amount < 2)
                        then
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
                            choose args.remaining args.black
                                |> Random.map
                                    (\piece ->
                                        { remaining = args.remaining - Piece.value piece
                                        , black = piece :: args.black
                                        }
                                    )
                    )
            )
            ({ remaining = lv * 2, black = [] }
                |> Random.constant
            )
        |> Random.andThen
            (\{ black } ->
                Level.fromPieces
                    { white = list
                    , black = black
                    }
            )
