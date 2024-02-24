module View.Summary exposing (..)

import Bug
import Collection exposing (Collection, Variant(..))
import Dict exposing (Dict)
import Game exposing (Tile(..))
import Html exposing (Html)
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
    [ Layout.divText [ Html.Style.fontSizePx 32 ] "Summary"
    , [ Layout.divText [] "Bugs found"
      , revealedBugs
            |> List.filterMap
                (\( pos, bug ) ->
                    Dict.get pos args.revealed
                        |> Maybe.map (Tuple.pair bug)
                )
            |> List.Extra.gatherEquals
            |> List.sortBy
                (\( ( bug, variant ), l ) ->
                    ( if Collection.member bug args.oldCollection then
                        1

                      else
                        0
                    , Collection.variantToString variant
                    , List.length l
                    )
                )
            |> List.map
                (\( ( bug, variant ), l ) ->
                    bug
                        |> Bug.toString
                        |> List.repeat (List.length l + 1)
                        |> String.concat
                        |> (if Collection.member bug args.oldCollection then
                                case variant of
                                    Cute ->
                                        View.Bubble.default []

                                    Royal ->
                                        View.Bubble.special []

                            else
                                case variant of
                                    Cute ->
                                        View.Bubble.new []

                                    Royal ->
                                        View.Bubble.newAndSpecial []
                           )
                )
            |> Html.div
                [ Html.Style.displayFlex
                , Html.Style.gapPx 8
                , Html.Style.justifyContentCenter
                ]
      ]
        |> Html.div
            [ Html.Style.displayFlex
            , Html.Style.flexDirectionColumn
            , Html.Style.gapPx 16
            , Html.Style.alignItemsCenter
            ]
    , [ Layout.divText [] "Bugs missed"
      , missedBugs
            |> List.map Tuple.second
            |> List.Extra.gatherEquals
            |> List.sortBy (\( _, l ) -> List.length l)
            |> List.map
                (\( bug, l ) ->
                    bug
                        |> Bug.toString
                        |> List.repeat (List.length l + 1)
                        |> String.concat
                        |> View.Bubble.unkown []
                )
            |> Html.div
                [ Html.Style.displayFlex
                , Html.Style.gapPx 8
                , Html.Style.justifyContentCenter
                ]
      ]
        |> Html.div
            [ Html.Style.displayFlex
            , Html.Style.flexDirectionColumn
            , Html.Style.gapPx 16
            , Html.Style.alignItemsCenter
            ]
    ]
        |> Html.div
            [ Html.Style.displayFlex
            , Html.Style.justifyContentCenter
            , Html.Style.alignItemsCenter
            , Html.Style.flexDirectionColumn
            , Html.Style.widthPx 352
            , Html.Style.backgroundColor "white"
            , Html.Style.borderRadius "8px"
            , Html.Style.positionAbsolute
            , Html.Style.top "50%"
            , Html.Style.left "50%"
            , Html.Style.transform "translate(-50%,-50%)"
            , Html.Style.paddingPx 64
            , Html.Style.boxSizingBorderBox
            , Html.Style.gapPx 32
            ]
