module AsteroidMiner.Page.Tutorial exposing (Model, Msg, areas, init, subscriptions, update)

import Action exposing (Action)
import AsteroidMiner.Building exposing (BuildingType(..), Code(..), Volume(..))
import AsteroidMiner.Data.Game as Game exposing (Game)
import AsteroidMiner.Data.Item as Item exposing (Item(..))
import AsteroidMiner.Lib.Map exposing (SquareType(..))
import AsteroidMiner.Page.Game as Game
import Dict
import Grid.Bordered as Grid
import PixelEngine exposing (Area)
import Random exposing (Seed)


type alias Model =
    { num : Int
    , content : Game.Model
    }


type alias Msg =
    Game.Msg


type alias TutorialAction =
    Action Model Never Never ()


maxTutorial : Int
maxTutorial =
    4


tutorial : Int -> Game -> Game
tutorial num game =
    (case num of
        1 ->
            [ ( ( 20, 20 ), Mine |> Game.newBuilding (Just Stone) )
            , ( ( 22, 20 ), Container Empty |> Game.newBuilding Nothing )

            --
            , ( ( 21, 18 ), ConveyorBelt Invalid |> Game.newBuilding Nothing )
            , ( ( 22, 18 ), Container Empty |> Game.newBuilding Nothing )

            --
            , ( ( 21, 16 ), ConveyorBelt Invalid |> Game.newBuilding Nothing )
            , ( ( 20, 16 ), Mine |> Game.newBuilding (Just Stone) )

            --
            , ( ( 20, 14 ), Mine |> Game.newBuilding (Just Stone) )
            , ( ( 21, 14 ), ConveyorBelt Invalid |> Game.newBuilding Nothing )
            , ( ( 22, 14 ), Container Empty |> Game.newBuilding Nothing )
            ]

        2 ->
            [ ( ( 20, 16 ), Mine |> Game.newBuilding (Just Stone) )
            , ( ( 21, 16 ), ConveyorBelt Invalid |> Game.newBuilding Nothing )
            , ( ( 22, 16 ), ConveyorBelt Invalid |> Game.newBuilding Nothing )
            , ( ( 23, 16 ), Container Empty |> Game.newBuilding Nothing )

            --
            , ( ( 20, 15 ), Mine |> Game.newBuilding (Just Stone) )
            , ( ( 23, 15 ), Container Empty |> Game.newBuilding Nothing )

            --
            , ( ( 20, 14 ), Mine |> Game.newBuilding (Just Stone) )
            , ( ( 23, 14 ), Container Empty |> Game.newBuilding Nothing )

            --
            , ( ( 19, 13 ), Mine |> Game.newBuilding (Just Stone) )
            , ( ( 20, 13 ), ConveyorBelt Invalid |> Game.newBuilding Nothing )
            , ( ( 21, 13 ), ConveyorBelt Invalid |> Game.newBuilding Nothing )
            , ( ( 22, 13 ), ConveyorBelt Invalid |> Game.newBuilding Nothing )
            , ( ( 23, 13 ), Container Empty |> Game.newBuilding Nothing )

            --
            , ( ( 18, 12 ), Mine |> Game.newBuilding (Just Stone) )
            , ( ( 23, 12 ), Container Empty |> Game.newBuilding Nothing )
            ]

        3 ->
            [ ( ( 20, 14 ), Mine |> Game.newBuilding (Just Stone) )
            , ( ( 21, 14 ), ConveyorBelt Invalid |> Game.newBuilding Nothing )
            , ( ( 22, 14 ), Container Empty |> Game.newBuilding Nothing )
            , ( ( 23, 14 ), Merger |> Game.newBuilding Nothing )
            , ( ( 23, 12 ), Container Empty |> Game.newBuilding Nothing )

            --
            , ( ( 20, 16 ), Mine |> Game.newBuilding (Just Stone) )
            , ( ( 21, 16 ), ConveyorBelt Invalid |> Game.newBuilding Nothing )
            , ( ( 22, 16 ), Container Empty |> Game.newBuilding Nothing )
            , ( ( 23, 16 ), Merger |> Game.newBuilding Nothing )
            , ( ( 23, 17 ), ConveyorBelt Invalid |> Game.newBuilding Nothing )
            , ( ( 23, 18 ), Container Empty |> Game.newBuilding Nothing )

            --
            , ( ( 19, 20 ), Mine |> Game.newBuilding (Just Stone) )
            , ( ( 20, 20 ), ConveyorBelt Invalid |> Game.newBuilding Nothing )
            , ( ( 21, 20 ), Container Empty |> Game.newBuilding Nothing )
            , ( ( 22, 20 ), Merger |> Game.newBuilding Nothing )
            , ( ( 22, 19 ), ConveyorBelt Invalid |> Game.newBuilding Nothing )
            , ( ( 22, 18 ), ConveyorBelt Invalid |> Game.newBuilding Nothing )

            --
            , ( ( 18, 12 ), Mine |> Game.newBuilding (Just Stone) )
            , ( ( 19, 12 ), ConveyorBelt Invalid |> Game.newBuilding (Just Stone) )
            , ( ( 20, 12 ), Container Empty |> Game.newBuilding Nothing )
            , ( ( 22, 12 ), ConveyorBelt Invalid |> Game.newBuilding Nothing )
            ]

        _ ->
            []
    )
        |> List.foldl
            (\( pos, building ) ->
                Grid.ignoringErrors
                    (Grid.update pos <| always <| Ok <| Just <| building)
            )
            game.map
        |> (\map ->
                { game
                    | map = map
                }
           )


init : Int -> Seed -> ( Model, Cmd msg )
init num seed =
    let
        ( content, cmd ) =
            seed |> Game.init
    in
    ( { num = num
      , content = { content | game = content.game |> tutorial num }
      }
    , cmd
    )


update : Msg -> Model -> TutorialAction
update msg ({ num, content } as model) =
    let
        ( maybeModel, cmd ) =
            Game.update msg content
                |> Action.config
                |> Action.withUpdate Just never
                |> Action.withExit ( Nothing, Cmd.none )
                |> Action.apply
    in
    case maybeModel of
        Just ({ inventory } as c) ->
            if
                inventory
                    |> Dict.get (Stone |> Item.toInt)
                    |> Maybe.withDefault 0
                    |> (\stone -> stone >= 256)
            then
                if num == maxTutorial then
                    Action.exiting

                else
                    Action.updating <|
                        init (num + 1) content.seed

            else
                Action.updating ( { model | content = c }, cmd )

        Nothing ->
            Action.exiting


subscriptions : Model -> Sub Msg
subscriptions { content } =
    content |> Game.subscriptions


areas : Model -> List (Area Msg)
areas { content } =
    content |> Game.areas
