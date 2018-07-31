module Roguelike.Main exposing (main)

--import Roguelike.Inventory as Inventory exposing (Inventory)

import Char
import Css exposing (px)
import Dict
import Html.Styled exposing (Html, program)
import Keyboard
import PixelEngine.Graphics as Graphics exposing (Area)
import PixelEngine.Graphics.Image as Image exposing (image)
import PixelEngine.Graphics.Tile as Tile exposing (Tile, Tileset)
import PixelEngine.ScreenTransition as Transition
import Random
import Roguelike.Cell as Cell
    exposing
        ( Cell(..)
        , ConsumableType(..)
        , EnemyType(..)
        , Item(..)
        , MiscellaneousType(..)
        , SolidType(..)
        )
import Roguelike.Game as Game
import Roguelike.Inventory as Inventory
import Roguelike.Map as Map exposing (Direction(..), Map)
import Roguelike.Player as Player exposing (PlayerCell, PlayerData)
import Roguelike.Tileset as Tileset


type GameType
    = Rogue
        { seed : Random.Seed
        , worldSeed : Int
        }
    | Tutorial Int
    | Menu


type alias Model =
    { map : Map Cell
    , oldScreen : Maybe (List (Area Msg))
    , player : PlayerData
    , gameType : GameType
    }


type Input
    = Direction Direction
    | Activate
    | RotateLeft
    | RotateRight


type Msg
    = Input Input
    | NextLevel
    | Idle


worldSize : Int
worldSize =
    16


init : Int -> ( Model, Cmd Msg )
init worldSeed =
    let
        backpackSize : Int
        backpackSize =
            8

        ( currentMap, currentSeed ) =
            Map.generate
                (worldSize - 1)
                Cell.mapGenerator
                (Random.initialSeed worldSeed)
                |> Tuple.mapFirst (Dict.update ( 7, 7 ) (always (Just (Player Down))))
    in
    { map = currentMap
    , oldScreen = Nothing
    , player = Player.init backpackSize
    , gameType =
        Rogue
            { worldSeed = worldSeed
            , seed = currentSeed
            }
    }
        ! [ Cmd.none ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ player, map, gameType} as model) =
    case msg of
        Input input ->
            let
                maybePlayer : Map Cell -> Maybe PlayerCell
                maybePlayer currentMap =
                    currentMap
                        |> Map.getUnique
                            (\_ cell ->
                                case cell of
                                    Player _ ->
                                        True

                                    _ ->
                                        False
                            )
                        |> Maybe.andThen
                            (\( key, cell ) ->
                                case cell of
                                    Player dir ->
                                        Just ( key, dir )

                                    _ ->
                                        Nothing
                            )
            in
            case maybePlayer map of
                Just playerCell ->
                    ( player, map )
                        |> (case input of
                                Activate ->
                                    Player.activate playerCell

                                Direction dir ->
                                    \game -> ( playerCell, game ) |> Game.applyDirection (worldSize - 1) dir |> Tuple.second

                                RotateLeft ->
                                    Tuple.mapFirst Player.rotateLeft

                                RotateRight ->
                                    Tuple.mapFirst Player.rotateRight
                           )
                        |> (\( playerData, newMap ) ->
                                { model
                                    | player = playerData
                                    , map = newMap
                                    , oldScreen = Nothing
                                }
                                    ! [ Cmd.none ]
                           )

                Nothing ->
                    case gameType of
                        Rogue {worldSeed} ->
                            init (worldSeed - 1)
                                |> Tuple.mapFirst
                                    (\newModel ->
                                        { newModel
                                            | oldScreen = Just deathScreen
                                        }
                                    )
                        _ -> model ! [ Cmd.none ]

        NextLevel ->
            case gameType of
                Rogue {worldSeed} ->
                    init (worldSeed + 7)
                        |> Tuple.mapFirst
                            (\({map,player} as newModel) ->
                                {newModel
                                | oldScreen = Just (worldScreen worldSeed map player )
                                }
                            )
                _ -> model ! [ Cmd.none ]

        Idle ->
            model ! [ Cmd.none ]


subscriptions : Model -> Sub Msg
subscriptions { map } =
    Keyboard.presses <|
        Char.fromCode
            >> (\char ->
                    if
                        map
                            |> Dict.toList
                            |> List.filter
                                (\( _, cell ) ->
                                    case cell of
                                        Enemy _ _ ->
                                            True

                                        _ ->
                                            False
                                )
                            |> List.isEmpty
                    then
                        NextLevel
                    else
                        case char of
                            'w' ->
                                Input (Direction Up)

                            's' ->
                                Input (Direction Down)

                            'd' ->
                                Input (Direction Right)

                            'a' ->
                                Input (Direction Left)

                            ' ' ->
                                Input Activate

                            'q' ->
                                Input RotateLeft

                            'e' ->
                                Input RotateRight

                            _ ->
                                Idle
               )


tileset : Tileset
tileset =
    Tile.tileset { source = "tileset.png", spriteHeight = 16, spriteWidth = 16 }


logo : Tileset
logo =
    Tile.tileset { source = "title_image", spriteHeight = 127, spriteWidth = 127 }


titleScreen : List (Area msg)
titleScreen =
    let
        scale : Int
        scale =
            2

        width : Int
        width =
            16

        tile : Tile msg
        tile =
            Tile.tile ( 0, 0 ) |> Tile.animated 1
    in
    [ Graphics.imageArea
        { height = toFloat <| scale * 20 * 16
        , background = Graphics.colorBackground (Css.rgb 20 12 28)
        }
        [ ( ( toFloat <| (scale * 16 * width) // 2 - 128, toFloat <| (scale * 12 * width) // 2 - 128 ), Image.fromTile tile logo )
        ]
    ]


deathScreen : List (Area msg)
deathScreen =
    let
        scale : Int
        scale =
            2

        width : Int
        width =
            16
    in
    [ Graphics.tiledArea
        { rows = 2
        , background = Graphics.colorBackground (Css.rgb 20 12 28)
        , tileset = tileset
        }
        []
    , Graphics.tiledArea
        { rows = 2
        , background = Graphics.colorBackground (Css.rgb 20 12 28)
        , tileset = tileset
        }
        [ ( ( 4, 0 ), Tileset.letter_y )
        , ( ( 5, 0 ), Tileset.letter_o )
        , ( ( 6, 0 ), Tileset.letter_u )
        , ( ( 8, 0 ), Tileset.letter_h )
        , ( ( 9, 0 ), Tileset.letter_a )
        , ( ( 10, 0 ), Tileset.letter_v )
        , ( ( 11, 0 ), Tileset.letter_e )
        , ( ( 6, 1 ), Tileset.letter_d )
        , ( ( 7, 1 ), Tileset.letter_i )
        , ( ( 8, 1 ), Tileset.letter_e )
        , ( ( 9, 1 ), Tileset.letter_d )
        ]
    , Graphics.imageArea
        { height = toFloat <| scale * 12 * 16
        , background = Graphics.colorBackground (Css.rgb 20 12 28)
        }
        [ ( ( toFloat <| (scale * 16 * width) // 2 - 128, toFloat <| (scale * 12 * width) // 2 - 128 ), image "skull.png" )
        ]
    , Graphics.tiledArea
        { rows = 2
        , background = Graphics.colorBackground (Css.rgb 20 12 28)
        , tileset = tileset
        }
        [ ( ( 4, 0 ), Tileset.letter_p )
        , ( ( 5, 0 ), Tileset.letter_r )
        , ( ( 6, 0 ), Tileset.letter_e )
        , ( ( 7, 0 ), Tileset.letter_s )
        , ( ( 8, 0 ), Tileset.letter_s )
        , ( ( 10, 0 ), Tileset.letter_a )
        , ( ( 11, 0 ), Tileset.letter_n )
        , ( ( 12, 0 ), Tileset.letter_y )
        , ( ( 6, 1 ), Tileset.letter_b )
        , ( ( 7, 1 ), Tileset.letter_u )
        , ( ( 8, 1 ), Tileset.letter_t )
        , ( ( 9, 1 ), Tileset.letter_t )
        , ( ( 10, 1 ), Tileset.letter_o )
        , ( ( 11, 1 ), Tileset.letter_n )
        ]
    , Graphics.tiledArea
        { rows = 2
        , background = Graphics.colorBackground (Css.rgb 20 12 28)
        , tileset = tileset
        }
        []
    ]


worldScreen : Int -> Map Cell -> PlayerData -> List (Area msg)
worldScreen worldSeed map player =
    [ Graphics.tiledArea
        { rows = 1
        , background = Graphics.colorBackground (Css.rgb 20 12 28)
        , tileset = tileset
        }
        ([ ( ( 6, 0 ), Tileset.letter_s )
         , ( ( 7, 0 ), Tileset.letter_c )
         , ( ( 8, 0 ), Tileset.letter_o )
         , ( ( 9, 0 ), Tileset.letter_r )
         , ( ( 10, 0 ), Tileset.letter_e )
         , ( ( 11, 0 ), Tileset.letter_colon )
         , ( ( 13, 0 ), Tileset.numberToTile (abs worldSeed // 100) )
         , ( ( 14, 0 ), Tileset.numberToTile ((abs worldSeed % 100) // 10) )
         , ( ( 15, 0 ), Tileset.numberToTile (abs worldSeed % 10) )
         ]
            |> (if (worldSeed // abs worldSeed) == -1 then
                    List.append [ ( ( 12, 0 ), Tileset.letter_minus ) ]
                else
                    List.append []
               )
        )
    , Graphics.tiledArea
        { rows = 16
        , background = Graphics.imageBackground { source = "groundTile.png", width = 16, height = 16 }
        , tileset = tileset
        }
        (map
            |> Dict.foldl
                (\pos cell list ->
                    ( pos
                    , Cell.getImage cell
                    )
                        :: list
                )
                []
        )
    , Graphics.tiledArea
        { rows = 3
        , background = Graphics.colorBackground (Css.rgb 20 12 28)
        , tileset = tileset
        }
        ([ ( ( 4, 2 ), Tileset.arrow_up )
         , ( ( 5, 2 ), Tileset.letter_s )
         , ( ( 6, 2 ), Tileset.letter_p )
         , ( ( 7, 2 ), Tileset.letter_a )
         , ( ( 8, 2 ), Tileset.letter_c )
         , ( ( 9, 2 ), Tileset.letter_e )
         , ( ( 10, 2 ), Tileset.letter_minus )
         , ( ( 11, 2 ), Tileset.letter_u )
         , ( ( 12, 2 ), Tileset.letter_s )
         , ( ( 13, 2 ), Tileset.letter_e )

         --
         , ( ( 0, 0 ), Tileset.arrow_down )
         , ( ( 1, 0 ), Tileset.letter_f )
         , ( ( 2, 0 ), Tileset.letter_l )
         , ( ( 3, 0 ), Tileset.letter_o )
         , ( ( 4, 0 ), Tileset.letter_o )
         , ( ( 5, 0 ), Tileset.letter_r )
         , ( ( 2, 1 ), Tileset.letter_q )
         , ( ( 3, 1 ), Tileset.arrow_left )
         , ( ( 12, 1 ), Tileset.arrow_right )
         , ( ( 13, 1 ), Tileset.letter_e )
         ]
            |> List.append
                (case player.inventory |> Inventory.ground of
                    Just a ->
                        [ ( ( 0, 1 ), Cell.getImage (Item a) ) ]

                    Nothing ->
                        []
                )
            |> List.append
                (List.range 0 (player.lifes - 1)
                    |> List.map (\i -> ( ( 15 - i, 0 ), Tileset.heart ))
                )
            |> List.append
                (player.inventory
                    |> Inventory.get
                    |> List.indexedMap
                        (\i a ->
                            ( ( 4 + i, 1 ), Cell.getImage (Item a) )
                        )
                )
        )
    ]


view : Model -> Html Msg
view ({oldScreen,gameType,player,map} as model) =
    let
        scale : Int
        scale =
            2

        width : Int
        width =
            16

        options =
            { scale = toFloat <| scale
            , width = toFloat <| scale * tileset.spriteWidth * width
            , transitionSpeedInSec = 0.2
            }
    in
    case gameType of
    Rogue {worldSeed} ->
        case oldScreen of
            Just justOldScreen ->
                { name = "next_level"
                , animation =
                    [ ( 0, "overflow:hidden;width:" ++ (toString <| scale * tileset.spriteWidth * width) ++ "px;" )
                    , ( 2, "overflow:hidden;width:0px;" )
                    ]
                }
                    |> Transition.apply
                        options
                        { from = justOldScreen
                        , to = worldScreen worldSeed map player
                        }

            Nothing ->
                if model.player.lifes > 0 then
                    Graphics.render options (worldScreen worldSeed map player)
                else
                    { name = "death_transition"
                    , animation =
                        [ ( 0, "opacity:1;filter:grayscale(0%) blur(0px);" )
                        , ( 1, "opacity:1;filter:grayscale(70%) blur(0px);" )
                        , ( 3, "opacity:0;filter:grayscale(70%) blur(5px);" )
                        ]
                    }
                        |> Transition.apply
                            options
                            { from = worldScreen worldSeed map player
                            , to = deathScreen
                            }
    _ ->
        Graphics.render options []


main : Program Never Model Msg
main =
    program
        { init = init 0
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
