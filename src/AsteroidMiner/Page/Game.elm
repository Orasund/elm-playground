module AsteroidMiner.Page.Game exposing (Model, Msg(..), areas, init, subscriptions, update)

import Action exposing (Action)
import AsteroidMiner.Building as Building exposing (BuildingType(..), Code(..), Volume(..))
import AsteroidMiner.Data exposing (floorCosts, fps, size, spriteSize)
import AsteroidMiner.Data.Comet as Comet exposing (Comet)
import AsteroidMiner.Data.Game as Game exposing (Game)
import AsteroidMiner.Data.Item as Item exposing (Item(..))
import AsteroidMiner.Data.Map as Game exposing (GroundType(..), Map, Square)
import AsteroidMiner.Lib.Command as Command exposing (idle)
import AsteroidMiner.Lib.Map as Map exposing (SquareType(..))
import AsteroidMiner.Lib.Neighborhood as Neighborhood
import AsteroidMiner.View as View exposing (ToolSelection(..))
import AsteroidMiner.View.GUI as GUI
import AsteroidMiner.View.Map as Map
import AsteroidMiner.View.Tileset as Tileset exposing (tileset)
import Color
import Dict exposing (Dict)
import Grid.Bordered as Grid exposing (Error(..))
import Grid.Position exposing (Position)
import Location exposing (Angle(..))
import PixelEngine exposing (Area)
import PixelEngine.Tile exposing (Tile)
import Random exposing (Seed)
import Time



----------------------
-- Model
----------------------


type alias Model =
    { game : Game
    , seed : Seed
    , gui : GUI.Model
    , inventory : Dict Int Int
    }


type Msg
    = TimePassed
    | SquareClicked Position
    | GuiSpecific GUI.Msg


type alias GameAction =
    Action Model Never Never ()



----------------------
-- Init
----------------------


init : Seed -> ( Model, Cmd msg )
init oldSeed =
    let
        center : Int
        center =
            size // 2

        ( angle, seed ) =
            oldSeed
                |> Random.step (Random.float 0 (2 * pi) |> Random.map Angle)

        comet : Comet
        comet =
            Comet.new angle

        map : Map
        map =
            Grid.fill
                (\( x, y ) ->
                    if (x - center) ^ 2 + (y - center) ^ 2 <= 8 ^ 2 then
                        Just <|
                            if (x + 1 - center) ^ 2 + (y - 1 - center) ^ 2 <= 6 ^ 2 then
                                ( GroundSquare <| Mountain, Nothing )

                            else
                                ( GroundSquare <| Dirt, Nothing )

                    else
                        Nothing
                )
                { rows = size
                , columns = size
                }

        game : Game
        game =
            { comet = comet
            , map = map
            , bag = Nothing
            , debts = Dict.empty
            }
    in
    ( { game = game
      , seed = seed
      , gui = GUI.init
      , inventory = Dict.empty
      }
    , Cmd.none
    )



----------------------
-- Update
----------------------


timePassed : Model -> GameAction
timePassed ({ game, seed } as model) =
    let
        ( ( comet, map ), newSeed ) =
            Random.step (game.comet |> Comet.update game.map) seed

        updateMap : Map -> Map
        updateMap =
            Map.update
                { empty = Dirt
                , update =
                    \pos ->
                        case Neighborhood.fromPosition pos game.map of
                            Ok ( Just ( BuildingSquare { value, sort }, maybeItem ), neigh ) ->
                                Game.updateBuilding
                                    sort
                                    { value = value, item = maybeItem }
                                    (neigh
                                        |> Neighborhood.map
                                            (Maybe.andThen Game.getBuildingType)
                                    )

                            _ ->
                                Command.idle
                , canStore =
                    \pos ->
                        case Neighborhood.fromPosition pos game.map of
                            Ok ( Just ( BuildingSquare { sort }, _ ), neigh ) ->
                                always <|
                                    Game.solveConflict
                                        sort
                                        (neigh
                                            |> Neighborhood.map
                                                (Maybe.andThen Game.getBuildingType)
                                        )

                            _ ->
                                always <| always <| always <| False
                }

        newGame : Game
        newGame =
            { game
                | comet = comet
                , map = updateMap map
            }
    in
    Action.updating
        ( { model
            | game = newGame
            , seed = newSeed
            , inventory = newGame.map |> Game.takeInventoryOfMap game.debts
          }
        , Cmd.none
        )


deleteSqaure : Position -> Model -> GameAction
deleteSqaure pos ({ game } as model) =
    let
        updateFun : Maybe Square -> Result () (Maybe Square)
        updateFun maybeElem =
            case maybeElem of
                Just ( BuildingSquare building, maybeItem ) ->
                    if building.sort |> Building.canBreak then
                        Ok <| Just <| Game.emptySquare maybeItem

                    else
                        Err ()

                _ ->
                    Err ()
    in
    Action.updating
        ( { model
            | game =
                { game
                    | map =
                        game.map
                            |> (Grid.ignoringErrors <| Grid.update pos updateFun)
                }
          }
        , Cmd.none
        )


placeSquare : BuildingType -> Position -> Model -> GameAction
placeSquare building position ({ game } as model) =
    let
        defaultCase : GameAction
        defaultCase =
            Action.updating
                ( model, Cmd.none )

        updateSquare : BuildingType -> Maybe Square -> Result () (Maybe Square)
        updateSquare b maybeSquare =
            case maybeSquare of
                Just ( GroundSquare Mountain, _ ) ->
                    Ok <|
                        Just <|
                            Game.newBuilding (Just Stone) b

                Just ( _, maybeItem ) ->
                    Ok <|
                        Just <|
                            Game.newBuilding maybeItem b

                Nothing ->
                    Err ()
    in
    case game.map |> Neighborhood.fromPosition position of
        Ok ( Just _, _ ) ->
            case game.map |> Grid.update position (updateSquare building) of
                Ok m ->
                    Action.updating
                        ( { model
                            | game = { game | map = m }
                          }
                        , Cmd.none
                        )

                Err _ ->
                    defaultCase

        _ ->
            defaultCase


pickUpSquare : Position -> Model -> GameAction
pickUpSquare position ({ gui, game } as model) =
    let
        newModel : Model
        newModel =
            case
                game.map
                    |> Grid.get position
            of
                Ok (Just ( square, Just item )) ->
                    { model
                        | game =
                            { game
                                | bag = Just item
                                , map =
                                    game.map
                                        |> Grid.ignoringErrors
                                            (Grid.update position
                                                (always <|
                                                    Ok <|
                                                        Just <|
                                                            ( square, Nothing )
                                                )
                                            )
                            }
                        , gui = gui |> GUI.select (Bag <| Just <| item)
                    }

                _ ->
                    model
    in
    Action.updating
        ( newModel
        , Cmd.none
        )


insertItem : Item -> Position -> Model -> GameAction
insertItem item position ({ gui, game } as model) =
    let
        newModel : Model
        newModel =
            case
                game.map
                    |> Grid.get position
            of
                Ok (Just ( BuildingSquare ({ value } as b), Just i )) ->
                    if i == item then
                        { model
                            | game =
                                { game
                                    | bag = Nothing
                                    , map =
                                        game.map
                                            |> Grid.ignoringErrors
                                                (Grid.update position
                                                    (always <|
                                                        Ok <|
                                                            Just <|
                                                                ( BuildingSquare { b | value = b.value + 1 }, Just i )
                                                    )
                                                )
                                }
                            , gui = gui |> GUI.select (Bag <| Nothing)
                        }

                    else
                        model

                _ ->
                    model
    in
    Action.updating
        ( newModel
        , Cmd.none
        )


squareClicked : Position -> Model -> GameAction
squareClicked position ({ gui, game } as model) =
    let
        build : BuildingType -> GameAction
        build tool =
            placeSquare tool position model

        placeFloor : GameAction
        placeFloor =
            Action.updating
                ( { model
                    | game =
                        { game
                            | map =
                                game.map
                                    |> Grid.ignoringErrors
                                        (Grid.insert position ( GroundSquare Dirt, Nothing ))
                            , debts =
                                game.debts
                                    |> Dict.update (Stone |> Item.toInt)
                                        (Maybe.map ((+) floorCosts)
                                            >> Maybe.withDefault floorCosts
                                            >> Just
                                        )
                        }
                  }
                , Cmd.none
                )
    in
    case gui.selected of
        View.Delete ->
            deleteSqaure position model

        View.Bag Nothing ->
            pickUpSquare position model

        View.Bag (Just item) ->
            insertItem item position model

        View.Mine ->
            Building.Mine |> build

        View.ConveyorBelt ->
            Building.ConveyorBelt Invalid |> build

        View.Container ->
            Building.Container Empty |> build

        View.Merger ->
            Building.Merger |> build

        View.Floor ->
            placeFloor


update : Msg -> Model -> GameAction
update msg =
    case msg of
        TimePassed ->
            timePassed

        SquareClicked position ->
            squareClicked position

        GuiSpecific guiMsg ->
            \({ gui } as model) ->
                Action.updating
                    ( { model | gui = GUI.update guiMsg gui }
                    , Cmd.none
                    )



----------------------
-- Subscriptions
----------------------


subscriptions : Model -> Sub Msg
subscriptions _ =
    let
        second : Float
        second =
            1000
    in
    Time.every (second / fps) (always TimePassed)



----------------------
-- Areas
----------------------


viewComet : Comet -> ( Position, Tile msg )
viewComet comet =
    ( Comet.position comet, Tileset.comet )


areas : Model -> List (Area Msg)
areas ({ game, gui } as model) =
    let
        { map, comet, bag, debts } =
            game

        inventory =
            model.inventory
                |> Dict.toList
                |> List.map (Tuple.mapFirst Item.fromInt)
    in
    [ PixelEngine.tiledArea
        { rows = size - 3
        , tileset = tileset
        , background =
            PixelEngine.imageBackground
                { source = "background.png"
                , width = spriteSize
                , height = spriteSize
                }
        }
      <|
        List.concat
            [ Map.view
                { onClick = SquareClicked
                , selected = gui.selected
                , inventory = inventory
                }
                map
            , [ viewComet comet ]
            ]
    , PixelEngine.imageArea
        { height = 3 * spriteSize
        , background =
            PixelEngine.colorBackground <|
                Color.rgb255 20 12 28
        }
        (GUI.view bag inventory gui)
        |> PixelEngine.mapArea GuiSpecific
    ]
