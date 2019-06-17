module AsteroidMiner.Page.Game exposing (Model, Msg(..), areas, init, subscriptions, update)

import Action exposing (Action)
import AsteroidMiner.Building as Building exposing (BuildingType(..))
import AsteroidMiner.Data exposing (fps, size, spriteSize)
import AsteroidMiner.Data.Comet as Comet exposing (Comet)
import AsteroidMiner.Data.Game as Game exposing (Game)
import AsteroidMiner.Data.Map as Game exposing (GroundType(..), Item(..), Map, Square)
import AsteroidMiner.Lib.Command as Command exposing (idle)
import AsteroidMiner.Lib.Map as Map exposing (SquareType(..))
import AsteroidMiner.Lib.Neighborhood as Neighborhood
import AsteroidMiner.View.GUI as GUI
import AsteroidMiner.View.Map as Map
import AsteroidMiner.View.Tileset as Tileset exposing (tileset)
import Color
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
                            if abs (x + y - center * 2) < 5 then
                                ( GroundSquare <| Mountain, Nothing )

                            else
                                ( GroundSquare <| Empty, Nothing )

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
            }
    in
    ( { game = game
      , seed = seed
      , gui = GUI.init
      }
    , Cmd.none
    )



----------------------
-- Update
----------------------


timePassed : Model -> GameAction
timePassed ({ game } as model) =
    Action.updating
        ( { model
            | game =
                { game
                    | comet =
                        game.comet
                            |> Comet.update
                    , map =
                        game.map
                            |> Map.update
                                { empty = Empty
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
                }
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
                    case building.sort of
                        Mine ->
                            Err ()

                        _ ->
                            Ok <| Just <| Game.emptySquare maybeItem

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
placeSquare _ position ({ game, gui } as model) =
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

        updateMap : Map -> Result Error Map
        updateMap map =
            if Game.isValid gui.selected position map then
                gui.selected
                    |> Building.toolToBuilding
                    |> Maybe.map
                        (\b -> map |> Grid.update position (updateSquare b))
                    |> Maybe.withDefault (Err NotSuccessful)

            else
                Err NotSuccessful
    in
    case game.map |> Neighborhood.fromPosition position of
        Ok ( Just _, _ ) ->
            case game.map |> updateMap of
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


squareClicked : Position -> Model -> GameAction
squareClicked position ({ gui } as model) =
    case gui.selected |> Building.toolToBuilding of
        Just tool ->
            placeSquare tool position model

        Nothing ->
            deleteSqaure position model


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
areas { game, gui } =
    let
        { map, comet } =
            game
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
            [ Map.view { onClick = SquareClicked, selected = gui.selected } map
            , [ viewComet comet ]
            ]
    , PixelEngine.imageArea
        { height = 3 * spriteSize
        , background =
            PixelEngine.colorBackground <|
                Color.rgb255 20 12 28
        }
        (GUI.view gui)
        |> PixelEngine.mapArea GuiSpecific
    ]
