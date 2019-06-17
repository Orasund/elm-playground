module AsteroidMiner.Lib.Map exposing (Map, Square, SquareType(..), update)

import AsteroidMiner.Building exposing (Building)
import AsteroidMiner.Data exposing (maxValue)
import AsteroidMiner.Lib.Command as Command exposing (Command)
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


store : Position -> Maybe c -> Building a -> Map a b c -> Result Error (Map a b c)
store pos maybeItem ({ value } as building) =
    Grid.update pos <|
        always <|
            if
                (value < maxValue)
                    && (maybeItem /= Nothing)
            then
                Ok <|
                    Just <|
                        ( BuildingSquare { building | value = value + 1 }
                        , Nothing
                        )

            else
                Err ()


send : Position -> Maybe c -> { empty : b, lookUp : Map a b c, canStore : Position -> a -> c -> { value : Int, item : c } -> Bool } -> Building a -> Direction -> Map a b c -> Result Error (Map a b c)
send pos maybeItem { lookUp, canStore } ({ value } as building) direction m =
    let
        neighborPos : Position
        neighborPos =
            pos |> Position.move 1 direction

        updateNeighbor : Building a -> Maybe c -> Map a b c -> Result Error (Map a b c)
        updateNeighbor b maybeC =
            Grid.update neighborPos
                (always <|
                    Ok <|
                        Just <|
                            ( BuildingSquare b, maybeC )
                )

        solveConflict : Building a -> { newC : c, oldC : c } -> Map a b c -> Result Error (Map a b c)
        solveConflict b { newC, oldC } =
            if canStore neighborPos b.sort newC { value = b.value, item = oldC } then
                Grid.update neighborPos
                    (always <|
                        Ok <|
                            Just <|
                                ( BuildingSquare { b | value = value + 1 }, Just newC )
                    )

            else
                always <| Err NotSuccessful
    in
    case maybeItem of
        Nothing ->
            Err NotSuccessful

        Just item ->
            lookUp
                |> Grid.get neighborPos
                |> Result.andThen
                    (\maybeEntry ->
                        m
                            |> (case maybeEntry of
                                    Just ( BuildingSquare b, Nothing ) ->
                                        updateNeighbor b maybeItem

                                    Just ( BuildingSquare b, Just i ) ->
                                        solveConflict b { newC = item, oldC = i }

                                    _ ->
                                        always <| Err NotSuccessful
                               )
                    )
                |> Result.andThen
                    (Grid.update pos <|
                        always <|
                            let
                                _ =
                                    ( building.sort, maybeItem )
                            in
                            if
                                (value > 0)
                                    && (maybeItem /= Nothing)
                            then
                                Ok <|
                                    Just <|
                                        ( BuildingSquare { building | value = value - 1 }
                                        , maybeItem
                                        )

                            else
                                Ok <|
                                    Just <|
                                        ( BuildingSquare building, Nothing )
                    )


apply : Command a c -> Position -> Square a b c -> { empty : b, lookUp : Map a b c, canStore : Position -> a -> c -> { value : Int, item : c } -> Bool } -> (Map a b c -> Map a b c)
apply command pos ( squareType, maybeItem ) ({ empty } as config) =
    let
        transition : Building a -> a -> Map a b c -> Result Error (Map a b c)
        transition building sort =
            Grid.update pos <|
                always <|
                    Ok <|
                        Just <|
                            ( BuildingSquare { building | sort = sort, value = 0 }
                            , maybeItem
                            )

        create : Building a -> c -> Map a b c -> Result Error (Map a b c)
        create building item =
            Grid.update pos <|
                always <|
                    Ok <|
                        Just <|
                            ( BuildingSquare { building | value = 0 }
                            , Just item
                            )

        destroy : Building a -> Map a b c -> Result Error (Map a b c)
        destroy _ =
            Grid.update pos <|
                always <|
                    Ok <|
                        Just <|
                            ( GroundSquare empty
                            , maybeItem
                            )
    in
    case squareType of
        GroundSquare _ ->
            identity

        BuildingSquare building ->
            command
                |> Command.apply
                    { store = store pos maybeItem building
                    , send = send pos maybeItem config building
                    , transition = transition building
                    , create = create building
                    , destroy = destroy building
                    }


update : { empty : b, update : Position -> Command a c, canStore : Position -> a -> c -> { value : Int, item : c } -> Bool } -> Map a b c -> Map a b c
update fun map =
    map
        |> Grid.foldl
            (\pos maybeSquare ->
                case maybeSquare of
                    Just (( BuildingSquare _, _ ) as square) ->
                        apply (fun.update pos) pos square { empty = fun.empty, lookUp = map, canStore = fun.canStore }

                    _ ->
                        identity
            )
            map
