module AsteroidMiner.Data.Building.ConveyorBelt exposing (update)

import AsteroidMiner.Data.Building as Building exposing (BeltColor(..), BuildingType(..), Command(..))
import AsteroidMiner.Data.Game as Game exposing (Command, Item, Square)
import AsteroidMiner.Data.Map exposing (SquareType(..))
import AsteroidMiner.Data.Neighborhood exposing (Neighborhood)
import Grid.Direction as Direction exposing (Direction(..))


type Code
    = Invalid
    | InputFound
    | Try BeltColor
    | Failed BeltColor


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


codeToInt : Code -> Int
codeToInt code =
    case code of
        Invalid ->
            0

        InputFound ->
            1

        Try Blue ->
            2

        Failed Blue ->
            3

        Try Green ->
            4

        Failed Green ->
            5

        Try Red ->
            6

        Failed Red ->
            7

        Try Yellow ->
            8

        Failed Yellow ->
            9


intToCode : Int -> Code
intToCode int =
    case int of
        0 ->
            Invalid

        1 ->
            InputFound

        2 ->
            Try Blue

        3 ->
            Failed Blue

        4 ->
            Try Green

        5 ->
            Failed Green

        6 ->
            Try Red

        7 ->
            Failed Red

        8 ->
            Try Yellow

        9 ->
            Failed Yellow

        _ ->
            Invalid


updateInvalid :
    { friends : List ( Direction, Maybe Square )
    , outputs : List ( Direction, Maybe Square )
    , inputs : List ( Direction, Maybe Square )
    }
    -> Game.Command
updateInvalid { friends, outputs, inputs } =
    let
        defaultCase : Game.Command
        defaultCase =
            Set (codeToInt Invalid)

        nextStage : Game.Command
        nextStage =
            Set (codeToInt InputFound)

        connect : Game.Command
        connect =
            if
                friends
                    |> List.filterMap
                        (\( _, maybeSquare ) ->
                            case maybeSquare of
                                Just ( BuildingSquare { counter }, _ ) ->
                                    Just counter

                                _ ->
                                    Nothing
                        )
                    |> List.member (InputFound |> codeToInt)
            then
                nextStage

            else
                defaultCase

        getDirection : List ( Direction, Maybe Square ) -> Maybe Direction
        getDirection =
            List.head >> Maybe.map Tuple.first
    in
    case ( friends |> List.length, ( outputs |> List.length, inputs |> List.length ) ) of
        ( 2, ( 0, 0 ) ) ->
            connect

        ( 1, ( 1, 0 ) ) ->
            connect

        ( 1, ( 0, 1 ) ) ->
            nextStage

        _ ->
            defaultCase


updateInputFound :
    { friends : List ( Direction, Maybe Square )
    , outputs : List ( Direction, Maybe Square )
    , inputs : List ( Direction, Maybe Square )
    }
    -> Game.Command
updateInputFound { friends, outputs, inputs } =
    let
        resetCase : Game.Command
        resetCase =
            Set (codeToInt Invalid)

        defaultCase : Game.Command
        defaultCase =
            Idle

        nextStage : Game.Command
        nextStage =
            Set (codeToInt <| Failed Yellow)

        friendCounters : List Int
        friendCounters =
            friends
                |> List.filterMap
                    (\( _, maybeSquare ) ->
                        case maybeSquare of
                            Just ( BuildingSquare { counter }, _ ) ->
                                Just counter

                            _ ->
                                Nothing
                    )

        connect : Game.Command
        connect =
            if
                friendCounters
                    |> List.member (Failed Yellow |> codeToInt)
            then
                nextStage

            else if
                (friendCounters
                    |> List.member (InputFound |> codeToInt)
                )
                    || (friendCounters
                            |> List.member (Invalid |> codeToInt)
                       )
            then
                defaultCase

            else
                resetCase

        getDirection : List ( Direction, Maybe Square ) -> Maybe Direction
        getDirection =
            List.head >> Maybe.map Tuple.first

        initiate : Game.Command
        initiate =
            case ( getDirection friends, getDirection outputs ) of
                ( Just Up, Just Down ) ->
                    nextStage

                ( Just Down, Just Up ) ->
                    nextStage

                ( Just Left, Just Right ) ->
                    nextStage

                ( Just Right, Just Left ) ->
                    nextStage

                _ ->
                    defaultCase
    in
    case ( friends |> List.length, ( outputs |> List.length, inputs |> List.length ) ) of
        ( 2, ( 0, 0 ) ) ->
            connect

        ( 1, ( 1, 0 ) ) ->
            initiate

        ( 1, ( 0, 1 ) ) ->
            connect

        _ ->
            resetCase


updateFailedColor :
    BeltColor
    -> List ( Direction, Maybe Square )
    ->
        { friends : List ( Direction, Maybe Square )
        , outputs : List ( Direction, Maybe Square )
        , inputs : List ( Direction, Maybe Square )
        }
    -> Game.Command
updateFailedColor color enemies { friends, outputs, inputs } =
    let
        resetCase : Game.Command
        resetCase =
            Set (codeToInt Invalid)

        defaultCase : Game.Command
        defaultCase =
            Idle

        nextStage : Game.Command
        nextStage =
            Set (codeToInt <| Try (color |> nextColor))

        skipStage : Game.Command
        skipStage =
            Set (codeToInt <| Try (color |> nextColor |> nextColor))

        failedStage : Game.Command
        failedStage =
            Set (codeToInt <| Failed (color |> nextColor))

        friendCounters : List Int
        friendCounters =
            friends
                |> List.filterMap
                    (\( _, maybeSquare ) ->
                        case maybeSquare of
                            Just ( BuildingSquare { counter }, _ ) ->
                                Just counter

                            _ ->
                                Nothing
                    )

        connect : Game.Command
        connect =
            if
                friendCounters
                    |> List.member (Try (color |> nextColor) |> codeToInt)
            then
                nextStage |> Debug.log "next2"

            else if
                friendCounters
                    |> List.member (Try (color |> nextColor |> nextColor) |> codeToInt)
            then
                skipStage

            else if
                (friendCounters
                    |> List.member (Failed color |> codeToInt)
                )
                    || (friendCounters
                            |> List.member (Try color |> codeToInt)
                       )
                    || (friendCounters
                            |> List.member (InputFound |> codeToInt)
                       )
                    || (friendCounters
                            |> List.member (Failed (color |> prevColor) |> codeToInt)
                       )
            then
                defaultCase

            else
                resetCase
    in
    case ( friends |> List.length, ( outputs |> List.length, inputs |> List.length ), enemies |> List.length ) of
        ( 2, ( 0, 0 ), 0 ) ->
            connect

        ( 1, ( 1, 0 ), 0 ) ->
            connect

        ( 1, ( 0, 1 ), 0 ) ->
            nextStage |> Debug.log "next1"

        ( _, _, 1 ) ->
            failedStage |> Debug.log "failed1"

        ( _, _, 2 ) ->
            failedStage |> Debug.log "failed2"

        _ ->
            resetCase


updateTryColor :
    BeltColor
    -> List ( Direction, Maybe Square )
    ->
        { friends : List ( Direction, Maybe Square )
        , outputs : List ( Direction, Maybe Square )
        , inputs : List ( Direction, Maybe Square )
        }
    -> Game.Command
updateTryColor color enemies { friends, outputs, inputs } =
    let
        resetCase : Game.Command
        resetCase =
            Set (codeToInt Invalid)

        defaultCase : Game.Command
        defaultCase =
            Idle

        failedStage : Game.Command
        failedStage =
            Set (codeToInt <| Failed color)

        nextStage : Direction -> Game.Command
        nextStage dir =
            Transition (ConveyorBelt <| Just ( color, dir ))

        friendCounters : List Int
        friendCounters =
            friends
                |> List.filterMap
                    (\( _, maybeSquare ) ->
                        case maybeSquare of
                            Just ( BuildingSquare { counter }, _ ) ->
                                Just counter

                            _ ->
                                Nothing
                    )

        connect : Game.Command
        connect =
            if
                friendCounters
                    |> List.member (Failed color |> codeToInt)
            then
                failedStage

            else if
                (friendCounters
                    |> Debug.log "friends"
                    |> List.member (Try color |> codeToInt)
                )
                    || (friendCounters
                            |> List.member (Failed (color |> prevColor) |> codeToInt)
                       )
                    || (friendCounters
                            |> List.member (Failed (color |> prevColor |> prevColor) |> codeToInt)
                       )
            then
                defaultCase

            else
                resetCase |> Debug.log "reset1"
    in
    case ( friends |> List.length, ( outputs |> List.length, inputs |> List.length ), enemies |> List.length ) of
        ( 2, ( 0, 0 ), 0 ) ->
            connect

        ( 1, ( 1, 0 ), 0 ) ->
            case outputs of
                ( dir, _ ) :: _ ->
                    dir
                        |> Direction.flip
                        |> nextStage

                _ ->
                    resetCase |> Debug.log "reset2"

        ( 1, ( 0, 1 ), 0 ) ->
            connect

        ( 1, ( 0, 0 ), 1 ) ->
            case friends of
                ( dir, _ ) :: _ ->
                    dir
                        |> nextStage

                _ ->
                    resetCase |> Debug.log "reset3"

        ( 0, ( 0, 0 ), 2 ) ->
            resetCase |> Debug.log "reset4"

        ( 0, ( 1, 0 ), 1 ) ->
            case outputs of
                ( dir, _ ) :: _ ->
                    dir
                        |> Direction.flip
                        |> nextStage

                _ ->
                    resetCase |> Debug.log "reset5"

        ( 0, ( 0, 1 ), 1 ) ->
            case inputs of
                ( dir, _ ) :: _ ->
                    dir
                        |> nextStage

                _ ->
                    resetCase |> Debug.log "reset6"

        ( 2, ( 0, 0 ), 1 ) ->
            failedStage

        ( 1, ( 0, 1 ), 1 ) ->
            failedStage

        ( 1, ( 1, 0 ), 1 ) ->
            failedStage

        ( 2, ( 0, 0 ), 2 ) ->
            failedStage

        ( 1, ( 0, 1 ), 2 ) ->
            failedStage

        ( 1, ( 1, 0 ), 2 ) ->
            failedStage

        _ ->
            resetCase |> Debug.log "reset7"


updateColorless : { counter : Int, item : Maybe Item } -> Neighborhood (Maybe Square) -> Game.Command
updateColorless { counter, item } { up, left, right, down } =
    let
        list : List ( Direction, Maybe Square )
        list =
            [ ( Up, up ), ( Left, left ), ( Right, right ), ( Down, down ) ]

        friends : List ( Direction, Maybe Square )
        friends =
            list
                |> List.filter
                    (Tuple.second
                        >> Maybe.map (Game.isBuildingType <| ConveyorBelt Nothing)
                        >> Maybe.withDefault False
                    )

        outputs : List ( Direction, Maybe Square )
        outputs =
            list
                |> List.filter
                    (Tuple.second
                        >> Maybe.andThen Game.getBuildingType
                        >> Maybe.map Building.isOutput
                        >> Maybe.withDefault False
                    )

        inputs : List ( Direction, Maybe Square )
        inputs =
            list
                |> List.filter
                    (Tuple.second
                        >> Maybe.andThen Game.getBuildingType
                        >> Maybe.map Building.isInput
                        >> Maybe.withDefault False
                    )
    in
    { friends = friends, inputs = inputs, outputs = outputs }
        |> (case counter |> intToCode of
                Invalid ->
                    updateInvalid

                InputFound ->
                    updateInputFound

                Try color ->
                    updateTryColor color <|
                        (list
                            |> List.filter
                                (Tuple.second
                                    >> Maybe.andThen Game.getBuildingType
                                    >> Maybe.map (Building.isWorkingConveyorBelt color)
                                    >> Maybe.withDefault False
                                )
                        )

                Failed color ->
                    updateFailedColor color <|
                        (list
                            |> List.filter
                                (Tuple.second
                                    >> Maybe.andThen Game.getBuildingType
                                    >> Maybe.map (Building.isWorkingConveyorBelt (color |> nextColor))
                                    >> Maybe.withDefault False
                                )
                        )
           )


updateColorful : BeltColor -> Direction -> { counter : Int, item : Maybe Item } -> Neighborhood (Maybe Square) -> Game.Command
updateColorful color direction { counter, item } { up, left, right, down } =
    let
        list : List ( Direction, Maybe Square )
        list =
            [ ( Up, up ), ( Left, left ), ( Right, right ), ( Down, down ) ]

        resetCase : Game.Command
        resetCase =
            Transition <| ConveyorBelt Nothing

        friends : Int
        friends =
            List.concat
                [ list
                    |> List.filter
                        (Tuple.second
                            >> Maybe.map (Game.isBuildingType <| ConveyorBelt <| Nothing)
                            >> Maybe.withDefault False
                        )
                    |> List.filter
                        (\( _, maybeSquare ) ->
                            case maybeSquare of
                                Just ( BuildingSquare b, _ ) ->
                                    (b.counter == (Try color |> codeToInt))
                                        || (b.counter == (Failed color |> codeToInt))

                                _ ->
                                    False
                        )
                , list
                    |> List.filter
                        (Tuple.second
                            >> Maybe.andThen Game.getBuildingType
                            >> Maybe.map (Building.isWorkingConveyorBelt color)
                            >> Maybe.withDefault False
                        )
                , list
                    |> List.filter
                        (Tuple.second
                            >> Maybe.andThen Game.getBuildingType
                            >> Maybe.map Building.isOutput
                            >> Maybe.withDefault False
                        )
                , list
                    |> List.filter
                        (Tuple.second
                            >> Maybe.andThen Game.getBuildingType
                            >> Maybe.map Building.isInput
                            >> Maybe.withDefault False
                        )
                ]
                |> List.length
    in
    if friends >= 2 then
        Send direction

    else
        resetCase


update : Maybe ( BeltColor, Direction ) -> { counter : Int, item : Maybe Item } -> Neighborhood (Maybe Square) -> Game.Command
update maybeBeltColor =
    case maybeBeltColor of
        Just ( beltColor, dir ) ->
            updateColorful beltColor dir

        Nothing ->
            updateColorless
