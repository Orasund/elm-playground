module AsteroidMiner.Page.Game exposing (Model, Msg(..), areas, init, subscriptions, update)

import Action exposing (Action)
import AsteroidMiner.Data exposing (fps, size, spriteSize)
import AsteroidMiner.Data.Building exposing (BuildingType(..))
import AsteroidMiner.Data.Comet as Comet exposing (Comet)
import AsteroidMiner.Data.Game as Game exposing (Game, GroundType(..), Map)
import AsteroidMiner.Data.Map as Map exposing (SquareType(..))
import AsteroidMiner.Data.Neighborhood as Neighborhood exposing (Neighborhood)
import AsteroidMiner.View.GUI as GUI exposing (Blueprint)
import AsteroidMiner.View.Map as Map
import AsteroidMiner.View.Tileset as Tileset exposing (tileset)
import Color
import Grid.Bordered as Grid
import Grid.Position exposing (Position)
import Location exposing (Angle(..))
import PixelEngine exposing (Area)
import PixelEngine.Tile exposing (Tile)
import Random exposing (Generator, Seed)
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
                    if (x - center) ^ 2 + (y - center) ^ 2 <= 4 ^ 2 then
                        Just <|
                            if abs (x + y - center * 2) < 3 then
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
                }
          }
        , Cmd.none
        )


squareClicked : Position -> Model -> GameAction
squareClicked position ({ game, gui } as model) =
    let
        sort =
            case gui.selected of
                GUI.ConveyorBelt ->
                    ConveyorBelt Nothing

                GUI.Container ->
                    Container

        mapResult : Maybe Game.Item -> Result () (Maybe Game.SquareType) -> Result () (Maybe Game.Square)
        mapResult maybeItem =
            Result.map <|
                Maybe.map <|
                    \a ->
                        ( a
                        , maybeItem
                        )

        defaultCase : GameAction
        defaultCase =
            Action.updating
                ( model, Cmd.none )

        emptyGroundCase : Result () (Maybe Game.SquareType)
        emptyGroundCase =
            Ok <|
                Just <|
                    BuildingSquare
                        { counter = 0
                        , sort =
                            sort
                        }

        isConveyorBelt : Maybe Game.Square -> Bool
        isConveyorBelt maybeSquareType =
            case maybeSquareType of
                Just ( BuildingSquare building, _ ) ->
                    case building.sort of
                        ConveyorBelt _ ->
                            True

                        _ ->
                            False

                _ ->
                    False

        mountainGroundCase : Neighborhood (Maybe Game.Square) -> Result () (Maybe Game.SquareType)
        mountainGroundCase neigh =
            if [ neigh.up, neigh.left, neigh.right, neigh.down ] |> List.any isConveyorBelt then
                Ok <|
                    Just <|
                        BuildingSquare
                            { counter = 0
                            , sort = Mine
                            }

            else
                Err ()
    in
    case game.map |> Neighborhood.fromPosition position of
        Ok ( Just square, neigh ) ->
            let
                map =
                    game.map
                        |> (Grid.ignoringErrors <|
                                Grid.update position <|
                                    always <|
                                        case square of
                                            ( GroundSquare Empty, maybeItem ) ->
                                                emptyGroundCase
                                                    |> mapResult maybeItem

                                            ( GroundSquare Mountain, maybeItem ) ->
                                                mountainGroundCase neigh
                                                    |> mapResult maybeItem

                                            _ ->
                                                Err ()
                           )
            in
            Action.updating
                ( { model
                    | game = { game | map = map }
                  }
                , Cmd.none
                )

        _ ->
            defaultCase


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
            [ Map.view SquareClicked map
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
