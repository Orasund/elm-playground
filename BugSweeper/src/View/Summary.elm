module View.Summary exposing (..)

import BugSpecies
import Collection exposing (Collection, Variant)
import Dict exposing (Dict)
import Game exposing (Tile(..))
import Html exposing (Html)
import Html.Attributes
import Html.Style
import Layout
import List.Extra
import View.Bubble


toHtml :
    { tiles : Dict ( Int, Int ) Tile
    , revealed : Dict ( Int, Int ) Variant
    , oldCollection : Collection
    }
    -> Html msg
toHtml args =
    let
        ( revealedBugs, missedBugs ) =
            args.tiles
                |> Dict.toList
                |> List.filterMap
                    (\( pos, tile ) ->
                        case tile of
                            BugTile bug ->
                                Just ( pos, bug )

                            _ ->
                                Nothing
                    )
                |> List.partition (\( pos, _ ) -> Dict.member pos args.revealed)
    in
    [ Layout.text [ Html.Attributes.style "font-size" "32px" ] "Summary"
    , [ Layout.text [] "Bugs found"
      , revealedBugs
            |> List.map Tuple.second
            |> List.Extra.gatherEquals
            |> List.sortBy (\( _, l ) -> List.length l)
            |> List.map
                (\( bug, l ) ->
                    bug
                        |> BugSpecies.toString
                        |> List.repeat (List.length l + 1)
                        |> String.concat
                        |> (if Collection.member bug args.oldCollection then
                                View.Bubble.default []

                            else
                                View.Bubble.new []
                           )
                )
            |> Layout.row
                [ Html.Style.gap "8px"
                , Html.Style.justifyContentCenter
                ]
      ]
        |> Layout.column [ Html.Style.gap "16px", Html.Style.alignItemsCenter ]
    , [ Layout.text [] "Bugs missed"
      , missedBugs
            |> List.map Tuple.second
            |> List.Extra.gatherEquals
            |> List.sortBy (\( _, l ) -> List.length l)
            |> List.map
                (\( bug, l ) ->
                    bug
                        |> BugSpecies.toString
                        |> List.repeat (List.length l + 1)
                        |> String.concat
                        |> View.Bubble.unkown []
                )
            |> Layout.row
                [ Html.Style.gap "8px"
                , Html.Style.justifyContentCenter
                ]
      ]
        |> Layout.column [ Html.Style.gap "16px", Html.Style.alignItemsCenter ]
    ]
        |> Layout.column
            (Layout.centered
                ++ [ Html.Attributes.style "width" "352px"
                   , Html.Attributes.style "background-color" "white"
                   , Html.Attributes.style "border-radius" "8px"
                   , Html.Style.positionAbsolute
                   , Html.Style.top "50%"
                   , Html.Style.left "50%"
                   , Html.Attributes.style "transform" "translate(-50%,-50%)"
                   , Html.Attributes.style "padding" "64px"
                   , Html.Style.boxSizingBorderBox
                   , Html.Style.gap "32px"
                   ]
            )
