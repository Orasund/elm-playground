module View.Tab exposing (..)

import Config
import Data.Actor exposing (Actor)
import Data.Block
import Data.Bomb
import Data.Entity exposing (Entity)
import Data.Floor exposing (Floor)
import Data.Game exposing (Game)
import Data.Info
import Data.Item exposing (Item)
import Data.Minecart
import Data.Tile
import Data.World
import Data.Zoom
import Html exposing (Html)
import Html.Attributes as Attr
import Layout
import ListBag
import View.Button
import View.Info
import View.Item
import View.Tab.Settings
import View.Tile


type Tab
    = SettingTab
    | DetailTab
    | BuildTab


tabList : List Tab
tabList =
    [ SettingTab, DetailTab, BuildTab ]


toString : Tab -> String
toString tab =
    case tab of
        SettingTab ->
            "Settings"

        DetailTab ->
            "Detail"

        BuildTab ->
            "Build"


buildButton game args =
    let
        ( item, cost ) =
            args.cost

        gotAmount =
            game
                |> Data.Game.getTrain
                |> .items
                |> ListBag.count item
    in
    [ [ args.tile
            |> View.Tile.toHtml 1 (Data.Zoom.fromPercent 70)
      , Html.text args.build
      ]
        |> Layout.row []
    , [ String.fromInt cost
            ++ "/"
            ++ String.fromInt gotAmount
            |> Html.text
      , item |> View.Item.toHtml
      , View.Button.toHtml
            (if cost <= gotAmount then
                Just args.onPress

             else
                Nothing
            )
            "Build"
      ]
        |> Layout.row []
    ]
        |> Layout.row [ Layout.spaceBetween ]


buildActorButton buildActor args =
    { build = (Data.Info.fromActor args.actor).title
    , cost = args.cost
    , onPress = buildActor args
    , tile = Data.Tile.fromActor args.actor
    }


buildFloorButton buildBlock game args =
    { build = (Data.Info.fromFloor args.floor).title
    , cost = args.cost
    , onPress = buildBlock args
    , tile = Data.Tile.fromFloor ( -1, -1 ) game args.floor
    }


buildEntityButton buildBlock args =
    { build = (Data.Info.fromEntity args.entity).title
    , cost = args.cost
    , onPress = buildBlock args
    , tile = Data.Tile.fromEntity args.entity
    }


sidebar :
    { restart : msg
    , setVolume : Maybe Int -> msg
    , volume : Int
    , setZoom : Maybe Int -> msg
    , zoom : Int
    , destroyBlock : msg
    , buildActor : { cost : ( Item, Int ), actor : Actor } -> msg
    , buildEntity : { cost : ( Item, Int ), entity : Entity } -> msg
    , buildFloor : { cost : ( Item, Int ), floor : Floor } -> msg
    , setTab : Maybe Tab -> msg
    , tab : Maybe Tab
    }
    -> Game
    -> Html msg
sidebar args game =
    let
        selected =
            game.world
                |> Data.World.get game.selected
    in
    [ tabList
        |> List.map
            (\tab ->
                tab
                    |> toString
                    |> View.Button.toHtml
                        (if Just tab == args.tab then
                            Just (args.setTab Nothing)

                         else
                            tab
                                |> Just
                                |> args.setTab
                                |> Just
                        )
            )
        |> Layout.row [ Layout.spacing 8 ]
    , args.tab
        |> Maybe.map
            (\tab ->
                [ tab
                    |> toString
                    |> Html.text
                    |> Layout.heading3 []
                , case tab of
                    SettingTab ->
                        View.Tab.Settings.settings
                            { restart = args.restart
                            , setVolume = args.setVolume
                            , volume = args.volume
                            , setZoom = args.setZoom
                            , zoom = args.zoom
                            }

                    DetailTab ->
                        selected
                            |> Maybe.map
                                (\( block, _ ) ->
                                    (block
                                        |> Data.Info.fromBlock
                                        |> View.Info.toHtml
                                    )
                                        :: (if
                                                case block of
                                                    Data.Block.ActorBlock ( _, Data.Actor.Minecart _ ) ->
                                                        True

                                                    Data.Block.FloorBlock Data.Floor.Track ->
                                                        True

                                                    _ ->
                                                        False
                                            then
                                                "Destroy"
                                                    |> View.Button.toHtml (Just args.destroyBlock)
                                                    |> List.singleton

                                            else
                                                []
                                           )
                                        |> Layout.column []
                                )
                            |> Maybe.withDefault
                                (Html.text "Nothing selected")

                    BuildTab ->
                        [ { floor = Data.Floor.Track
                          , cost = ( Data.Item.Iron, Config.trackCost )
                          }
                            |> buildFloorButton args.buildFloor game
                            |> buildButton game
                        , { entity = Data.Entity.container
                          , cost = ( Data.Item.Iron, Config.containerCost )
                          }
                            |> buildEntityButton args.buildEntity
                            |> buildButton game
                        , { actor = Data.Actor.Minecart Data.Minecart.emptyWagon
                          , cost = ( Data.Item.Iron, Config.wagonCost )
                          }
                            |> buildActorButton args.buildActor
                            |> buildButton game
                        , { actor = Data.Actor.Bomb Data.Bomb.new
                          , cost = ( Data.Item.Gold, Config.bombCost )
                          }
                            |> buildActorButton args.buildActor
                            |> buildButton game
                        , "Destroy"
                            |> View.Button.toHtml (Just args.destroyBlock)
                        ]
                            |> Layout.column
                                [ Layout.spacing 8
                                ]
                ]
                    |> Layout.column
                        [ Layout.spacing 8
                        , Attr.style "width" "200px"
                        , Attr.style "background-color" "white"
                        , Attr.style "padding" "8px"
                        , Attr.style "border-radius" "8px"
                        , Attr.style "border" "solid 1px black"
                        ]
            )
        |> Maybe.withDefault Layout.none
    ]
        |> Layout.column [ Layout.spacing 8 ]
