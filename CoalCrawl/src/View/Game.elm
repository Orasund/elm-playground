module View.Game exposing (..)

import AnyBag
import Config
import Data.Actor exposing (Actor)
import Data.Block exposing (Block)
import Data.Floor
import Data.Game exposing (Game)
import Data.Info
import Data.Item exposing (Item)
import Data.Wagon
import Data.World
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Layout
import View.Button
import View.Info
import View.Screen


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


toHtml :
    { tileClicked : ( Int, Int ) -> msg
    , toggleSlowdown : msg
    , restart : msg
    , destroyBlock : msg
    , buildActor : { cost : ( Item, Int ), actor : Actor } -> msg
    , buildBlock : { cost : ( Item, Int ), block : Block } -> msg
    , camera : ( Int, Int )
    , slowedDown : Bool
    , setVolume : String -> msg
    , volume : Int
    }
    -> Game
    -> List (Html msg)
toHtml args game =
    [ game
        |> View.Screen.fromGame { onPress = args.tileClicked, camera = args.camera }
    , [ [ (if args.slowedDown then
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
      , Data.Info.fromTrain game |> View.Info.justContent
      , game.world
            |> Data.World.get game.selected
            |> Maybe.map
                (\block ->
                    [ block
                        |> Data.Info.fromBlock game
                        |> View.Info.toHtml
                    , Html.text "Build" |> Layout.heading4 []
                    ]
                        ++ (case block of
                                Data.Block.FloorBlock floor ->
                                    ([ { actor = Data.Actor.Wagon Data.Wagon.emptyWagon
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

                                _ ->
                                    []
                           )
                        |> Layout.column [ Layout.spacing 8 ]
                )
            |> Maybe.withDefault Layout.none
      ]
        |> Layout.column [ Layout.spacing 8, Attr.style "width" "300px" ]
    ]
