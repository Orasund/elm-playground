module Roguelike exposing (main)

import Char
import Css exposing (px)
import Dict exposing (Dict)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, src)
import Keyboard
import Random
import Roguelike.Cell as Cell exposing (Cell(..), ConsumableType(..), Direction(..), EnemyType(..), Item(..), SolidType(..))
import Roguelike.Inventory as Inventory exposing (Inventory)
import Roguelike.Map as Map exposing (Map)
import Roguelike.Player as Player exposing (PlayerData)
import SelectList exposing (Position, SelectList)


type alias Config =
    { worldSeed : Int
    , worldSize : Int
    }


type alias Model =
    { map : SelectList (Map Cell)
    , seed : Random.Seed
    , inventory : Inventory Item
    , config : Config
    , lifes : Int
    }


type Input
    = Direction Direction
    | Drop
    | Activate
    | RotateLeft
    | RotateRight


type Msg
    = Input Input
    | Idle


updateSelected : (a -> a) -> SelectList a -> SelectList a
updateSelected fun list =
    list
        |> SelectList.mapBy
            (\pos ->
                if pos == SelectList.Selected then
                    fun
                else
                    identity
            )


init : ( Model, Cmd Msg )
init =
    let
        worldSeed : Int
        worldSeed =
            31414

        worldSize : Int
        worldSize =
            16

        backpackSize : Int
        backpackSize =
            3

        ( map, currentSeed ) =
            Map.generate
                (worldSize - 1)
                (\pos ( map, seed ) ->
                    let
                        ( r, new_seed ) =
                            Random.step (Random.int 0 40) seed
                    in
                    if r < 12 then
                        ( map |> Dict.insert pos (Solid DirtWall)
                        , new_seed
                        )
                    else if r < 13 then
                        ( map |> Dict.insert pos (Item (Consumable Bombe))
                        , new_seed
                        )
                    else if r < 14 then
                        ( map |> Dict.insert pos (Item (Consumable Cheese))
                        , new_seed
                        )
                    else
                        ( map
                        , new_seed
                        )
                )
                (Random.initialSeed worldSeed)
                |> Tuple.mapFirst (Dict.update ( 7, 7 ) (always (Just (Player Down))))
                |> Tuple.mapFirst SelectList.singleton
    in
    { map = map
    , seed = currentSeed
    , config = { worldSeed = worldSeed, worldSize = worldSize }
    , inventory = Inventory.init backpackSize
    , lifes = 3
    }
        ! [ Cmd.none ]


attackPlayer : Model -> Model
attackPlayer model =
    { model | lifes = model.lifes - 1 }


movePlayer : Map.Location -> Direction -> Model -> Model
movePlayer ( x, y ) direction model =
    let
        ( i, j ) =
            Map.dirCoordinates direction

        outOfBound : Bool
        outOfBound =
            case direction of
                Up ->
                    y == 0

                Down ->
                    y == (model.config.worldSize - 1)

                Left ->
                    x == (model.config.worldSize - 1)

                Right ->
                    x == 0

        move : ( Int, Int ) -> ( Int, Int ) -> Map Cell -> Map Cell
        move ( x, y ) ( i, j ) map =
            map
                |> Dict.update ( x + i, y + j ) (always (Just (Player direction)))
                |> Dict.remove ( x, y )
    in
    if outOfBound then
        model
    else
        case model.map |> SelectList.selected |> Dict.get ( x + i, y + j ) of
            Just (Item a) ->
                let
                    ( item, inventory ) =
                        model.inventory |> Inventory.drop
                in
                { model
                    | map =
                        model.map
                            |> updateSelected (move ( x, y ) ( i, j ))
                            |> (\map ->
                                    case item of
                                        Just a ->
                                            map |> updateSelected (Map.place ( x, y ) (Item a))

                                        _ ->
                                            map
                               )
                    , inventory =
                        inventory |> Inventory.add a
                }

            Nothing ->
                let
                    ( item, inventory ) =
                        model.inventory |> Inventory.drop
                in
                { model
                    | map =
                        model.map
                            |> updateSelected (move ( x, y ) ( i, j ))
                            |> (\map ->
                                    case item of
                                        Just a ->
                                            map |> updateSelected (Map.place ( x, y ) (Item a))

                                        Nothing ->
                                            map
                               )
                    , inventory = inventory
                }

            _ ->
                { model
                    | map = model.map |> updateSelected (Player.face ( x, y ) direction)
                }


removeEnemy : Map.Location -> EnemyType -> Map Cell -> Map Cell
removeEnemy location enemyType map =
    map |> Map.remove location


updateEnemy : Map.Location -> EnemyType -> Model -> Model
updateEnemy ( i, j ) enemyType model =
    let
        player : Maybe PlayerData
        player =
            Player.get (model.map |> SelectList.selected)
    in
    (case player of
        Just ( ( x, y ), _ ) ->
            [ Up, Down, Left, Right ]
                |> List.filter (\dir -> ( i - x, j - y ) == Map.dirCoordinates dir)
                |> (\list ->
                        case list of
                            _ :: _ ->
                                model |> attackPlayer

                            _ ->
                                model
                   )

        Nothing ->
            model
    )
        |> (\model ->
                case enemyType of
                    PlacedBombe ->
                        { model | map = model.map |> updateSelected (removeEnemy ( i, j ) enemyType) }
           )



--_ ->
--    m


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input Drop ->
            let
                ( item, inventory ) =
                    model.inventory |> Inventory.take

                dir : Map.Location
                dir =
                    case Player.get (model.map |> SelectList.selected) of
                        Just ( ( x, y ), dir ) ->
                            let
                                ( i, j ) =
                                    Map.dirCoordinates dir
                            in
                            ( x + i, y + j )

                        Nothing ->
                            ( 0, 0 )
            in
            case model.map |> SelectList.selected |> Dict.get dir of
                Nothing ->
                    { model
                        | map =
                            case item of
                                Just a ->
                                    model.map
                                        |> updateSelected (Map.place dir (Item a))

                                _ ->
                                    model.map
                        , inventory = inventory
                    }
                        ! [ Cmd.none ]

                _ ->
                    model ! [ Cmd.none ]

        Input Activate ->
            case model.inventory |> Inventory.selected of
                Just (Consumable a) ->
                    ({ model | inventory = model.inventory |> Inventory.take |> Tuple.second }
                        |> (\model ->
                                case a of
                                    Bombe ->
                                        { model
                                            | map =
                                                model.map
                                                    |> updateSelected
                                                        (\map ->
                                                            let
                                                                pos : ( Int, Int )
                                                                pos =
                                                                    case Player.get map of
                                                                        Just ( ( x, y ), dir ) ->
                                                                            let
                                                                                ( i, j ) =
                                                                                    Map.dirCoordinates dir
                                                                            in
                                                                            ( x + i, y + j )

                                                                        Nothing ->
                                                                            ( 0, 0 )
                                                            in
                                                            case map |> Dict.get pos of
                                                                Nothing ->
                                                                    map |> Map.place pos (Enemy PlacedBombe)

                                                                _ ->
                                                                    map
                                                        )
                                        }

                                    _ ->
                                        model
                           )
                    )
                        ! [ Cmd.none ]

                _ ->
                    model ! [ Cmd.none ]

        Input (Direction a) ->
            let
                player : Maybe PlayerData
                player =
                    Player.get (model.map |> SelectList.selected)
            in
            (case player of
                Just ( ( x, y ), dir ) ->
                    if dir == a then
                        model.map
                            |> SelectList.selected
                            |> Dict.foldl
                                (\( i, j ) cell out ->
                                    case cell of
                                        Enemy b ->
                                            out |> updateEnemy ( i, j ) b

                                        _ ->
                                            out
                                )
                                (model |> movePlayer ( x, y ) a)
                    else
                        { model
                            | map =
                                model.map
                                    |> updateSelected (Player.face ( x, y ) a)
                        }

                Nothing ->
                    model
            )
                ! [ Cmd.none ]

        Input RotateLeft ->
            { model
                | inventory = model.inventory |> Inventory.rotateLeft
            }
                ! [ Cmd.none ]

        Input RotateRight ->
            { model
                | inventory = model.inventory |> Inventory.rotateRight
            }
                ! [ Cmd.none ]

        Idle ->
            model ! [ Cmd.none ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Keyboard.presses
        (Char.fromCode
            >> (\char ->
                    case char of
                        'w' ->
                            Input (Direction Up)

                        's' ->
                            Input (Direction Down)

                        'd' ->
                            Input (Direction Left)

                        'a' ->
                            Input (Direction Right)

                        'f' ->
                            Input Drop

                        'r' ->
                            Input Activate

                        'q' ->
                            Input RotateLeft

                        'e' ->
                            Input RotateRight

                        _ ->
                            Idle
               )
        )


tile : ( Int, Int ) -> ( number, number1 ) -> Html msg
tile ( x, y ) ( i, j ) =
    img
        [ src
            "test.png"
        , css
            [ Css.property "object-fit" "none"
            , Css.property
                "object-position"
                ("-" ++ toString (16 * i) ++ "px -" ++ toString (16 * j) ++ "px")
            , Css.width (px 16)
            , Css.height (px 16)
            , Css.position Css.absolute
            , Css.top (px (toFloat (32 * y + 8)))
            , Css.left (px (toFloat (32 * x + 8)))
            , Css.property "image-rendering" "pixelated"
            , Css.transform (Css.scale2 2 2)
            ]
        ]
        []


view : Model -> Html Msg
view model =
    div [ css [ Css.backgroundColor (Css.rgb 0 0 0) ] ]
        [ div
            [ css
                [ Css.width (px 512)
                , Css.height (px 512)
                , Css.margin Css.auto
                , Css.position Css.relative
                , Css.backgroundImage (Css.url "background.png")
                , Css.backgroundRepeat Css.repeat
                , Css.backgroundSize2 (px 32) (px 32)
                , Css.property "image-rendering" "pixelated"
                ]
            ]
            (model.map
                |> SelectList.selected
                |> Dict.foldl
                    (\pos cell list ->
                        tile pos (Cell.getImage cell) :: list
                    )
                    []
            )
        , div
            [ css
                [ Css.width (px 512)
                , Css.height (px (32 * 3))
                , Css.margin Css.auto
                , Css.position Css.relative
                , Css.backgroundColor (Css.rgb 20 12 28)
                ]
            ]
            ([ tile ( 2, 0 ) ( 11, 13 ) --\/
             , tile ( 3, 0 ) ( 1, 13 ) --f
             , tile ( 4, 0 ) ( 10, 15 ) -- -
             , tile ( 5, 0 ) ( 0, 15 ) --d
             , tile ( 6, 0 ) ( 4, 13 ) --r
             , tile ( 7, 0 ) ( 3, 14 ) --o
             , tile ( 8, 0 ) ( 3, 15 ) --p
             , tile ( 2, 2 ) ( 11, 12 ) --/\
             , tile ( 3, 2 ) ( 4, 13 ) --r
             , tile ( 4, 2 ) ( 10, 15 ) -- -
             , tile ( 5, 2 ) ( 0, 12 ) --a
             , tile ( 6, 2 ) ( 0, 14 ) --c
             , tile ( 7, 2 ) ( 4, 15 ) --t
             , tile ( 8, 2 ) ( 2, 12 ) --i
             , tile ( 9, 2 ) ( 5, 13 ) --v
             , tile ( 10, 2 ) ( 0, 12 ) --a
             , tile ( 11, 2 ) ( 4, 15 ) --t
             , tile ( 12, 2 ) ( 1, 12 ) --e

             --
             , tile ( 12, 0 ) ( 4, 12 ) --q
             , tile ( 13, 0 ) ( 11, 15 ) --<
             , tile ( 14, 0 ) ( 11, 14 ) -->
             , tile ( 15, 0 ) ( 1, 12 ) --e
             ]
                |> List.append
                    (case model.inventory |> Inventory.ground of
                        Just a ->
                            [ tile ( 0, 1 ) (Cell.getImage (Item a)) ]

                        Nothing ->
                            []
                    )
                |> List.append
                    (List.range 0 (model.lifes - 1)
                        |> List.map (\i -> tile ( 15 - i, 2 ) ( 4, 8 ))
                    )
                |> List.append
                    (model.inventory
                        |> Inventory.get
                        |> List.indexedMap
                            (\i a ->
                                tile ( 2 + i, 1 ) (Cell.getImage (Item a))
                            )
                    )
            )
        ]


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
