module View.Tab exposing (..)

import Config
import Data.Actor exposing (Actor)
import Data.Block exposing (Block)
import Data.Bomb
import Data.Entity
import Data.Floor
import Data.Game exposing (Game)
import Data.Info
import Data.Item exposing (Item)
import Data.Minecart
import Data.World
import Html exposing (Html)
import Html.Attributes as Attr
import Layout
import ListBag
import View.Button
import View.Info
import View.Tab.Settings


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
    [ Html.text args.build
    , "Build for "
        ++ String.fromInt cost
        ++ " "
        ++ String.fromChar (Data.Item.toChar item)
        |> View.Button.toHtml
            (if cost <= gotAmount then
                Just args.onPress

             else
                Nothing
            )
    ]
        |> Layout.row [ Layout.spaceBetween ]


buildActorButton buildActor args =
    { build = (Data.Info.fromActor args.actor).title
    , cost = args.cost
    , onPress = buildActor args
    }


buildBlockButton buildBlock game args =
    { build = (Data.Info.fromBlock game args.block).title
    , cost = args.cost
    , onPress = buildBlock args
    }


sidebar :
    { restart : msg
    , setVolume : Maybe Int -> msg
    , volume : Int
    , setZoom : Maybe Int -> msg
    , zoom : Int
    , destroyBlock : msg
    , buildActor : { cost : ( Item, Int ), actor : Actor } -> msg
    , buildBlock : { cost : ( Item, Int ), block : Block } -> msg
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
                                        |> Data.Info.fromBlock game
                                        |> View.Info.toHtml
                                    )
                                        :: (if
                                                case block of
                                                    Data.Block.EntityBlock (Data.Entity.Actor id) ->
                                                        case game.world |> Data.World.getActor id of
                                                            Just ( _, Data.Actor.Minecart _ ) ->
                                                                True

                                                            _ ->
                                                                False

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
                        (([ { actor = Data.Actor.Minecart Data.Minecart.emptyWagon
                            , cost = ( Data.Item.Iron, Config.wagonCost )
                            }
                                |> buildActorButton args.buildActor
                          , { actor = Data.Actor.Bomb Data.Bomb.new
                            , cost = ( Data.Item.Gold, Config.bombCost )
                            }
                                |> buildActorButton args.buildActor
                          ]
                            |> List.map (buildButton game)
                         )
                            ++ ([ { block = Data.Floor.Track |> Data.Block.FloorBlock
                                  , cost = ( Data.Item.Iron, Config.trackCost )
                                  }
                                    |> buildBlockButton args.buildBlock game
                                ]
                                    |> List.map (buildButton game)
                               )
                            ++ [ "Destroy"
                                    |> View.Button.toHtml (Just args.destroyBlock)
                               ]
                        )
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
