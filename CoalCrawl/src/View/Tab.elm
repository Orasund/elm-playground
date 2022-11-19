module View.Tab exposing (..)

import AnyBag
import Config
import Data.Actor exposing (Actor)
import Data.Block exposing (Block)
import Data.Entity
import Data.Floor
import Data.Game exposing (Game)
import Data.Info
import Data.Item exposing (Item)
import Data.Wagon
import Data.World
import Html exposing (Html)
import Html.Attributes as Attr
import Layout
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
            game.train.items
                |> AnyBag.count item
    in
    [ Html.text args.build
    , "Build for "
        ++ String.fromInt cost
        ++ " "
        ++ Data.Item.toString item
        |> View.Button.toHtml
            (if cost <= gotAmount then
                Just args.onPress

             else
                Nothing
            )
    , "You got "
        ++ String.fromInt gotAmount
        ++ " "
        ++ Data.Item.toString item
        ++ "."
        |> Html.text
    ]
        |> Layout.row [ Layout.spacing 8 ]


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
    { toggleSlowdown : msg
    , slowedDown : Bool
    , restart : msg
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
                            , slowedDown = args.slowedDown
                            , setVolume = args.setVolume
                            , toggleSlowdown = args.toggleSlowdown
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
                                        :: (case block of
                                                Data.Block.EntityBlock (Data.Entity.Actor id) ->
                                                    case game.world |> Data.World.getActor id of
                                                        Just ( _, Data.Actor.Wagon wagon ) ->
                                                            if AnyBag.isEmpty wagon.items then
                                                                "Destroy"
                                                                    |> View.Button.toHtml (Just args.destroyBlock)
                                                                    |> List.singleton

                                                            else
                                                                []

                                                        _ ->
                                                            []

                                                Data.Block.FloorBlock Data.Floor.Track ->
                                                    "Destroy"
                                                        |> View.Button.toHtml (Just args.destroyBlock)
                                                        |> List.singleton

                                                _ ->
                                                    []
                                           )
                                        |> Layout.column []
                                )
                            |> Maybe.withDefault
                                (Html.text "Nothing selected")

                    BuildTab ->
                        case selected of
                            Just ( Data.Block.FloorBlock floor, _ ) ->
                                (([ { actor = Data.Actor.Wagon Data.Wagon.emptyWagon
                                    , cost = ( Data.Item.Iron, Config.wagonCost )
                                    }
                                        |> buildActorButton args.buildActor
                                  , { actor = Data.Actor.bomb
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
                                )
                                    |> Layout.column
                                        [ Layout.spacing 8
                                        ]

                            _ ->
                                "You can only build on empty floor tiles" |> Html.text
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
