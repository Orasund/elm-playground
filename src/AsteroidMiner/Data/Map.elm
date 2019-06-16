module AsteroidMiner.Data.Map exposing (Map, Square, SquareType(..), update)

import AsteroidMiner.Data.Building exposing (Building, Command(..))
import Grid.Bordered as Grid exposing (Error(..), Grid)
import Grid.Direction exposing (Direction)
import Grid.Position as Position exposing (Position)


type SquareType a b
    = GroundSquare b
    | BuildingSquare (Building a)


type alias Square a b c =
    ( SquareType a b, Maybe c )


type alias Map a b c =
    Grid (Square a b c)


apply : Command a -> Position -> Square a b c -> Map a b c -> Result Error (Map a b c)
apply command pos ( squareType, maybeItem ) map =
    let
        decrease : Building a -> Map a b c -> Result Error (Map a b c)
        decrease ({ counter } as building) =
            if counter == 0 then
                always <| Err NotSuccessful

            else
                Grid.update pos <|
                    always <|
                        Ok <|
                            Just <|
                                ( BuildingSquare
                                    { building
                                        | counter = counter - 1
                                    }
                                , maybeItem
                                )

        set : Int -> Building a -> Map a b c -> Result Error (Map a b c)
        set num building =
            Grid.update pos <|
                always <|
                    Ok <|
                        Just <|
                            ( BuildingSquare
                                { building | counter = num }
                            , maybeItem
                            )

        send : Direction -> Building a -> Map a b c -> Result Error (Map a b c)
        send direction building =
            Grid.update (pos |> Position.move 1 direction)
                (\maybeEntry ->
                    case maybeEntry of
                        Just ( BuildingSquare b, Nothing ) ->
                            Ok <|
                                Just <|
                                    ( BuildingSquare b, maybeItem )

                        _ ->
                            Err ()
                )
                >> Result.andThen
                    (Grid.update pos <|
                        always <|
                            Ok <|
                                Just <|
                                    ( BuildingSquare building, Nothing )
                    )

        transition : a -> Building a -> Map a b c -> Result Error (Map a b c)
        transition sort building =
            Grid.update pos <|
                always <|
                    Ok <|
                        Just <|
                            ( BuildingSquare { building | sort = sort, counter = 0 }
                            , maybeItem
                            )
    in
    map
        |> (case squareType of
                GroundSquare _ ->
                    Ok

                BuildingSquare building ->
                    case command of
                        Idle ->
                            Ok

                        Decrease ->
                            decrease building

                        Set int ->
                            set int building

                        Send direction ->
                            send direction building

                        Transition sort ->
                            transition sort building
           )


update : (Position -> Command a) -> Map a b c -> Map a b c
update getCommand map =
    map
        |> Grid.foldl
            (\pos maybeSquare ->
                case maybeSquare of
                    Just (( BuildingSquare _, _ ) as square) ->
                        Grid.ignoringErrors <|
                            apply (getCommand pos) pos square

                    _ ->
                        identity
            )
            map
