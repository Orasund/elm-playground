module AsteroidMiner.Building.ConveyorBelt exposing (canStore, update)

import AsteroidMiner.Building as Building exposing (BeltColor(..), BuildingType(..), Code(..))
import AsteroidMiner.Data.Map exposing (Command, Item, Neighborhood)
import AsteroidMiner.Lib.Command exposing (idle, send, transition)
import AsteroidMiner.Lib.Map exposing (SquareType(..))
import AsteroidMiner.Lib.Neighborhood as Neighborhood
import Grid.Direction as Direction exposing (Direction(..))


canStore : Neighborhood -> Item -> { value : Int, item : Item } -> Bool
canStore _ _ _ =
    False


prevColor : BeltColor -> BeltColor
prevColor color =
    case color of
        Blue ->
            Yellow

        Green ->
            Blue

        Red ->
            Green

        Yellow ->
            Red


nextColor : BeltColor -> BeltColor
nextColor color =
    case color of
        Blue ->
            Green

        Green ->
            Red

        Red ->
            Yellow

        Yellow ->
            Blue


areOpposing : ( Direction, Maybe BuildingType ) -> ( Direction, Maybe BuildingType ) -> Bool
areOpposing a b =
    case
        ( a |> Tuple.first
        , b |> Tuple.first
        )
    of
        ( Up, Down ) ->
            True

        ( Down, Up ) ->
            True

        ( Left, Right ) ->
            True

        ( Right, Left ) ->
            True

        _ ->
            False


updateInvalid :
    { friends : List ( Direction, Maybe BuildingType )
    , outputs : List ( Direction, Maybe BuildingType )
    , inputs : List ( Direction, Maybe BuildingType )
    }
    -> Command
updateInvalid { friends, outputs, inputs } =
    let
        defaultCase : Command
        defaultCase =
            transition (ConveyorBelt Invalid)

        nextStage : Command
        nextStage =
            transition (ConveyorBelt InputFound)

        lastStage : Command
        lastStage =
            transition (ConveyorBelt <| Try Blue)

        recoverStage : BeltColor -> Command
        recoverStage =
            Try >> ConveyorBelt >> transition

        hasGoodFriend : List ( Direction, Maybe BuildingType ) -> Maybe BeltColor
        hasGoodFriend =
            List.foldl
                (\a maybe ->
                    case maybe of
                        Nothing ->
                            case a of
                                ( _, Just (ConveyorBelt (Try b)) ) ->
                                    Just b

                                _ ->
                                    Nothing

                        Just _ ->
                            maybe
                )
                Nothing

        connect : Command
        connect =
            case friends |> hasGoodFriend of
                Just color ->
                    recoverStage color

                Nothing ->
                    if
                        friends
                            |> List.any (\( _, a ) -> a == (Just <| ConveyorBelt InputFound))
                    then
                        nextStage

                    else
                        defaultCase
    in
    case ( friends |> List.length, ( outputs |> List.length, inputs |> List.length ) ) of
        ( 2, ( _, _ ) ) ->
            connect

        ( 1, ( 0, 0 ) ) ->
            defaultCase

        ( 1, ( _, 0 ) ) ->
            connect

        ( 1, ( 0, 1 ) ) ->
            nextStage

        ( 0, ( 1, 1 ) ) ->
            lastStage

        _ ->
            defaultCase


updateInputFound :
    { friends : List ( Direction, Maybe BuildingType )
    , outputs : List ( Direction, Maybe BuildingType )
    , inputs : List ( Direction, Maybe BuildingType )
    }
    -> Command
updateInputFound { friends, outputs, inputs } =
    let
        resetCase : Command
        resetCase =
            transition (ConveyorBelt Invalid)

        defaultCase : Command
        defaultCase =
            idle

        nextStage : Command
        nextStage =
            transition (ConveyorBelt <| Failed Yellow)

        connect : Command
        connect =
            if
                friends
                    |> List.any
                        (Tuple.second
                            >> Maybe.map
                                ((==) (ConveyorBelt <| Failed Yellow))
                            >> Maybe.withDefault False
                        )
            then
                nextStage

            else if
                friends
                    |> List.any
                        (Tuple.second
                            >> Maybe.map
                                (\a ->
                                    (a == ConveyorBelt InputFound)
                                        || (a == ConveyorBelt Invalid)
                                )
                            >> Maybe.withDefault False
                        )
            then
                defaultCase

            else
                resetCase

        getDirection : List ( Direction, a ) -> Maybe Direction
        getDirection =
            List.head >> Maybe.map Tuple.first

        initiate : Command
        initiate =
            case friends |> List.head of
                Just f ->
                    if outputs |> List.any (areOpposing f) then
                        nextStage

                    else
                        defaultCase

                Nothing ->
                    defaultCase
    in
    case ( friends |> List.length, ( outputs |> List.length, inputs |> List.length ) ) of
        ( 2, ( _, _ ) ) ->
            connect

        ( 1, ( 0, 0 ) ) ->
            resetCase

        ( 1, ( _, 0 ) ) ->
            initiate

        ( 1, ( 0, 1 ) ) ->
            connect

        _ ->
            resetCase


updateFailedColor :
    BeltColor
    -> List ( Direction, Maybe BuildingType )
    ->
        { friends : List ( Direction, Maybe BuildingType )
        , outputs : List ( Direction, Maybe BuildingType )
        , inputs : List ( Direction, Maybe BuildingType )
        }
    -> Command
updateFailedColor color enemies { friends, outputs, inputs } =
    let
        resetCase : Command
        resetCase =
            transition (ConveyorBelt Invalid)

        defaultCase : Command
        defaultCase =
            idle

        nextStage : Command
        nextStage =
            transition (ConveyorBelt <| Try (color |> nextColor))

        skipStage : Command
        skipStage =
            transition (ConveyorBelt <| Try (color |> nextColor |> nextColor))

        failedStage : Command
        failedStage =
            transition (ConveyorBelt <| Failed (color |> nextColor))

        connect : Command
        connect =
            if
                friends
                    |> List.any
                        (Tuple.second
                            >> Maybe.map
                                ((==) (ConveyorBelt <| Try (color |> nextColor)))
                            >> Maybe.withDefault False
                        )
            then
                nextStage

            else if
                friends
                    |> List.any
                        (Tuple.second
                            >> Maybe.map
                                ((==) (ConveyorBelt <| Try (color |> nextColor |> nextColor)))
                            >> Maybe.withDefault False
                        )
            then
                skipStage

            else if
                friends
                    |> List.any
                        (Tuple.second
                            >> Maybe.map
                                (\a ->
                                    (a == (ConveyorBelt <| Failed color))
                                        || (a == (ConveyorBelt <| Try color))
                                        || (a == (ConveyorBelt <| InputFound))
                                        || (a == (ConveyorBelt <| Failed (color |> prevColor)))
                                        || (a == (ConveyorBelt <| Failed (color |> prevColor |> prevColor)))
                                )
                            >> Maybe.withDefault False
                        )
            then
                defaultCase

            else
                defaultCase

        --resetCase
    in
    case ( friends |> List.length, ( outputs |> List.length, inputs |> List.length ), enemies |> List.length ) of
        ( 1, ( 0, 1 ), 0 ) ->
            nextStage

        ( 0, ( 1, 1 ), _ ) ->
            nextStage

        ( _, _, 1 ) ->
            failedStage

        ( _, _, 2 ) ->
            failedStage

        _ ->
            connect


updateTryColor :
    BeltColor
    -> List ( Direction, Maybe BuildingType )
    ->
        { friends : List ( Direction, Maybe BuildingType )
        , outputs : List ( Direction, Maybe BuildingType )
        , inputs : List ( Direction, Maybe BuildingType )
        }
    -> Command
updateTryColor color enemies { friends, outputs, inputs } =
    let
        resetCase : Command
        resetCase =
            transition (ConveyorBelt Invalid)

        defaultCase : Command
        defaultCase =
            idle

        failedStage : Command
        failedStage =
            transition (ConveyorBelt <| Failed color)

        nextStage : Direction -> Command
        nextStage dir =
            transition (ColoredConveyorBelt color dir)

        connect : Command
        connect =
            if
                friends
                    |> List.any
                        (Tuple.second
                            >> Maybe.map ((==) (ConveyorBelt <| Failed color))
                            >> Maybe.withDefault False
                        )
            then
                failedStage

            else if
                friends
                    |> List.any
                        (Tuple.second
                            >> Maybe.map
                                (\a ->
                                    (a == (ConveyorBelt <| Try color))
                                        || (a == (ConveyorBelt <| Failed (color |> prevColor)))
                                        || (a == (ConveyorBelt <| Failed (color |> prevColor |> prevColor)))
                                )
                            >> Maybe.withDefault False
                        )
            then
                defaultCase

            else
                resetCase

        getDirection : List ( Direction, a ) -> Maybe Direction
        getDirection =
            List.head >> Maybe.map Tuple.first

        create : Command
        create =
            case ( getDirection inputs, getDirection outputs ) of
                ( Just Up, Just Down ) ->
                    nextStage Up

                ( Just Down, Just Up ) ->
                    nextStage Down

                ( Just Left, Just Right ) ->
                    nextStage Left

                ( Just Right, Just Left ) ->
                    nextStage Right

                _ ->
                    connect
    in
    if
        (friends |> List.length)
            + (outputs |> List.length)
            + (inputs |> List.length)
            + (enemies |> List.length)
            >= 2
    then
        case ( friends |> List.length, ( outputs |> List.length, inputs |> List.length ), enemies |> List.length ) of
            ( 2, ( _, _ ), 0 ) ->
                connect

            ( 1, ( 0, 0 ), 0 ) ->
                connect

            ( 1, ( _, 0 ), 0 ) ->
                case friends |> List.head of
                    Just f ->
                        case outputs |> List.filter (areOpposing f) |> List.head of
                            Just ( dir, _ ) ->
                                dir
                                    |> Direction.flip
                                    |> nextStage

                            Nothing ->
                                connect

                    Nothing ->
                        connect

            ( 1, ( 0, 1 ), 0 ) ->
                connect

            ( 1, ( _, _ ), 1 ) ->
                case friends of
                    ( dir, _ ) :: _ ->
                        dir
                            |> nextStage

                    _ ->
                        connect

            ( 0, ( 1, 0 ), 1 ) ->
                case outputs of
                    ( dir, _ ) :: _ ->
                        dir
                            |> Direction.flip
                            |> nextStage

                    _ ->
                        connect

            ( 0, ( 0, 1 ), 1 ) ->
                case inputs of
                    ( dir, _ ) :: _ ->
                        dir
                            |> nextStage

                    _ ->
                        connect

            ( _, ( 1, 1 ), 0 ) ->
                create

            ( _, ( 1, 1 ), _ ) ->
                failedStage

            _ ->
                connect

    else
        failedStage


update : Code -> Neighborhood -> Command
update code neigh =
    let
        friends : List ( Direction, Maybe BuildingType )
        friends =
            neigh |> Neighborhood.filter Building.isConveyorBelt

        outputs : List ( Direction, Maybe BuildingType )
        outputs =
            neigh |> Neighborhood.filter Building.isOutput

        inputs : List ( Direction, Maybe BuildingType )
        inputs =
            neigh |> Neighborhood.filter Building.isInput
    in
    { friends = friends, inputs = inputs, outputs = outputs }
        |> (case code of
                Invalid ->
                    updateInvalid

                InputFound ->
                    updateInputFound

                Try color ->
                    updateTryColor color <|
                        (neigh
                            |> Neighborhood.filter
                                (Building.isConveyorBeltColored color)
                        )

                Failed color ->
                    updateFailedColor color <|
                        (neigh
                            |> Neighborhood.filter
                                (Building.isConveyorBeltColored (color |> nextColor))
                        )
           )
