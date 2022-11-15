module View.Sidebar exposing (..)

import AnyBag
import Config
import Data.Actor exposing (Actor)
import Data.Block exposing (Block)
import Data.Floor
import Data.Game exposing (Game)
import Data.Info
import Data.Item exposing (Item)
import Data.Train
import Data.Wagon
import Data.World
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Layout
import View.Button
import View.Info


type SidebarTab
    = SettingTab
    | DetailTab
    | BuildTab


tabList : List SidebarTab
tabList =
    [ SettingTab, DetailTab, BuildTab ]


toString : SidebarTab -> String
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


settings :
    { restart : msg
    , slowedDown : Bool
    , setVolume : String -> msg
    , toggleSlowdown : msg
    , volume : Int
    }
    -> Html msg
settings args =
    [ [ (if args.slowedDown then
            "Stop Slow Motion"

         else
            "Start Slow Motion"
        )
            |> View.Button.toHtml (Just args.toggleSlowdown)
      , View.Button.toHtml (Just args.restart) "Restarts"
      ]
        |> Layout.row [ Layout.spacing 8 ]
    , [ Html.text "Volume"
      , Html.input
            [ Attr.type_ "range"
            , Attr.min "0"
            , Attr.max "100"
            , Attr.value (String.fromInt args.volume)
            , Events.onInput args.setVolume
            ]
            []
      ]
        |> Layout.row [ Layout.spacing 8 ]
    ]
        |> Layout.column [ Layout.spacing 8 ]


sidebar :
    { toggleSlowdown : msg
    , slowedDown : Bool
    , restart : msg
    , setVolume : String -> msg
    , volume : Int
    , destroyBlock : msg
    , buildActor : { cost : ( Item, Int ), actor : Actor } -> msg
    , buildBlock : { cost : ( Item, Int ), block : Block } -> msg
    , setTab : Maybe SidebarTab -> msg
    , tab : Maybe SidebarTab
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
                        settings
                            { restart = args.restart
                            , slowedDown = args.slowedDown
                            , setVolume = args.setVolume
                            , toggleSlowdown = args.toggleSlowdown
                            , volume = args.volume
                            }

                    DetailTab ->
                        selected
                            |> Maybe.map
                                (\block ->
                                    block
                                        |> Data.Info.fromBlock game
                                        |> View.Info.toHtml
                                )
                            |> Maybe.withDefault
                                (Html.text "Nothing selected")

                    BuildTab ->
                        case selected of
                            Just (Data.Block.FloorBlock floor) ->
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
                                    ++ (case floor of
                                            Data.Floor.Ground _ ->
                                                [ { block = Data.Floor.Track |> Data.Block.FloorBlock
                                                  , cost = ( Data.Item.Iron, Config.trackCost )
                                                  }
                                                    |> buildBlockButton args.buildBlock game
                                                ]
                                                    |> List.map (buildButton game)

                                            _ ->
                                                []
                                       )
                                    ++ (case floor of
                                            Data.Floor.Track ->
                                                "Destroy"
                                                    |> View.Button.toHtml (Just args.destroyBlock)
                                                    |> List.singleton

                                            _ ->
                                                []
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
                        , Attr.style "width" "300px"
                        , Attr.style "background-color" "white"
                        , Attr.style "padding" "8px"
                        , Attr.style "border-radius" "16px"
                        , Attr.style "border" "solid 1px black"
                        ]
            )
        |> Maybe.withDefault Layout.none
    ]
        |> Layout.column
            [ Layout.spacing 8
            , Attr.style "padding" "8px"
            , Attr.style "position" "absolute"
            , Attr.style "top" "0"
            , Attr.style "left" "0"
            ]
