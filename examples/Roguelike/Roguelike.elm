module Roguelike exposing (main)

--import Roguelike.Inventory as Inventory exposing (Inventory)

import Char
import Css exposing (px)
import Dict exposing (Dict)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, src)
import Keyboard
import Random
import Roguelike.Cell as Cell exposing (Cell(..),MiscellaneousType(..), ConsumableType(..), Direction(..), EnemyType(..), Item(..), SolidType(..))
import Roguelike.Game as Game exposing (Game)
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
    , player : PlayerData
    , seed : Random.Seed
    , config : Config
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
            31412

        worldSize : Int
        worldSize =
            16

        backpackSize : Int
        backpackSize =
            8

        ( map, currentSeed ) =
            Map.generate
                (worldSize - 1)
                (\pos ( map, seed ) ->
                    let
                        ( r, new_seed ) =
                            Random.step (Random.int 0 100) seed
                    in
                    if r < 40 then
                        ( map |> Dict.insert pos (Solid DirtWall)
                        , new_seed
                        )
                    else if r < 45 then
                        ( map |> Dict.insert pos (Item (Consumable Bombe))
                        , new_seed
                        )
                    else if r < 46 then
                        ( map |> Dict.insert pos (Item (Consumable Cheese))
                        , new_seed
                        )
                    else if r < 47 then
                        ( map |> Dict.insert pos (Enemy Rat)
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
    , player = Player.init backpackSize
    }
        ! [ Cmd.none ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input input ->
            let
                ( playerData, map ) =
                    case input of
                        Drop ->
                            ( model.player, model.map |> SelectList.selected )
                                |> Player.drop

                        Activate ->
                            ( model.player, model.map |> SelectList.selected )
                                |> Player.activate

                        Direction dir ->
                            ( model.player, model.map |> SelectList.selected )
                                |> Game.applyDirection (model.config.worldSize - 1) dir

                        RotateLeft ->
                            ( model.player, model.map |> SelectList.selected )
                                |> Player.rotateLeft

                        RotateRight ->
                            ( model.player, model.map |> SelectList.selected )
                                |> Player.rotateRight
            in
            { model
                | player = playerData
                , map = model.map |> updateSelected (always map)
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

                        ' ' ->
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
             , tile ( 3, 2 ) ( 4, 14 ) --s
             , tile ( 4, 2 ) ( 3, 15 ) -- p
             , tile ( 5, 2 ) ( 0, 12 ) --a
             , tile ( 6, 2 ) ( 0, 14 ) --c
             , tile ( 7, 2 ) ( 1, 12 ) --e
             , tile ( 8, 2 ) ( 10, 15 ) -- -
             , tile ( 9, 2 ) ( 5, 12 ) -- u
             , tile ( 10, 2 ) ( 4, 14 ) --s
             , tile ( 11, 2 ) ( 1, 12 ) --e

             --
             , tile ( 12, 0 ) ( 4, 12 ) --q
             , tile ( 13, 0 ) ( 11, 15 ) --<
             , tile ( 14, 0 ) ( 11, 14 ) -->
             , tile ( 15, 0 ) ( 1, 12 ) --e
             ]
                |> List.append
                    (case model.player.inventory |> Inventory.ground of
                        Just a ->
                            [ tile ( 0, 1 ) (Cell.getImage (Item a)) ]

                        Nothing ->
                            []
                    )
                |> List.append
                    (List.range 0 (model.player.lifes - 1)
                        |> List.map (\i -> tile ( 15 - i, 2 ) ( 4, 8 ))
                    )
                |> List.append
                    (model.player.inventory
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
