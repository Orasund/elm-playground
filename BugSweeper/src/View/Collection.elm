module View.Collection exposing (closedCollection, detailCard, openCollection)

import Bug exposing (Bug)
import Collection exposing (Collection, Variant(..))
import Html exposing (Attribute, Html)
import Html.Attributes
import Html.Events
import Html.Style
import Layout
import Object
import View.Bubble
import View.Variant


maxDrawerHeight =
    180


minDrawerHeight =
    66


drawer : List (Attribute msg) -> List (Html msg) -> Html msg
drawer attrs =
    Html.div
        ([ Html.Style.flexDirectionColumn
         , Html.Style.widthPx 352
         , Html.Style.backgroundColor "white"
         , Html.Style.borderTopLeftRadiusPx 8
         , Html.Style.borderTopRightRadiusPx 8
         , Html.Style.boxSizingBorderBox
         ]
            ++ attrs
        )


card : List (Attribute msg) -> List (Html msg) -> Html msg
card attrs =
    Html.div
        ([ Html.Style.flexDirectionColumn
         , Html.Style.backgroundColor "white"
         , Html.Style.borderRadius "16px"
         , Html.Style.widthPx 200
         , Html.Style.aspectRatio "2/3"
         , Html.Style.alignItemsCenter
         , Html.Style.justifyContentCenter
         ]
            ++ attrs
        )


detailCard : ( Bug, Variant ) -> Html msg
detailCard ( bug, variant ) =
    [ [ Layout.divText [ Html.Style.fontSizePx 64 ]
            (Bug.toString bug)
      , [ "Found next to " |> Layout.divText []
        , (Bug.requirementsOf bug
            |> List.concatMap
                (\( n, tile ) ->
                    tile
                        |> Maybe.map Object.toString
                        |> Maybe.withDefault "❌"
                        |> List.repeat n
                )
            |> String.concat
          )
            |> Layout.divText [ Html.Style.justifyContentCenter ]
        ]
            |> Html.div
                [ Html.Style.flexDirectionColumn
                , Html.Style.gapPx 8
                ]
      ]
        |> card [ Html.Style.gapPx 32 ]
    , case variant of
        Cute ->
            Layout.divWrapper [] Layout.none

        Royal ->
            View.Variant.royal
                [ Html.Style.borderRadius "16px"
                ]
    ]
        |> Html.div
            [ Html.Style.justifyContentCenter
            , Html.Style.alignItemsCenter
            , Html.Style.positionAbsolute
            , Html.Style.top ("calc(50% - " ++ String.fromFloat (maxDrawerHeight / 2) ++ "px)")
            , Html.Style.left "50%"
            , Html.Style.transform "translate(-50%,-50%)"
            ]


openCollection :
    List (Attribute msg)
    ->
        { onSelect : ( Bug, Variant ) -> msg
        }
    -> Collection
    -> Html msg
openCollection attrs args collectedBugs =
    [ "Your collection"
        |> Layout.divText
            [ Html.Style.padding "8px 16px"
            , Html.Style.justifyContentCenter
            ]
    , (Bug.list
        |> List.map
            (\species ->
                case collectedBugs |> Collection.get species of
                    [] ->
                        Bug.toString species
                            |> View.Bubble.unkown []

                    list ->
                        (if List.member Royal list then
                            View.Bubble.specialButton []

                         else
                            View.Bubble.button []
                        )
                            { label = "View details of " ++ Bug.toString species
                            , onPress =
                                Just
                                    (args.onSelect
                                        ( species
                                        , if List.member Royal list then
                                            Royal

                                          else
                                            Cute
                                        )
                                    )
                            }
                            (Bug.toString species)
            )
      )
        |> Html.div
            [ Html.Style.paddingPx 16
            , Html.Style.fontSizePx 20
            , Html.Style.alignItemsCenter
            , Html.Style.gapPx 16
            ]
    ]
        |> drawer
            ([ Html.Style.positionFixed
             , Html.Style.bottomPx 0
             , Html.Style.heightPx maxDrawerHeight
             , Html.Style.transition "height 0.2s"
             ]
                ++ attrs
            )


closedCollection :
    List (Attribute msg)
    -> { onOpen : msg }
    -> Collection
    -> Html msg
closedCollection attrs args collectedBugs =
    [ [ "Your collection"
            |> Layout.divText
                [ Html.Style.alignItemsCenter
                ]
      , (collectedBugs
            |> Collection.bugs
            |> List.map Bug.toString
            |> String.concat
        )
            |> Layout.divText
                [ Html.Attributes.style "font-size" "20px"
                , Html.Style.alignItemsCenter
                ]
      ]
        |> Html.div
            [ Html.Style.flexDirectionColumn
            , Html.Style.heightPx minDrawerHeight
            , Html.Style.alignItemsCenter
            , Html.Style.paddingPx 8
            , Html.Style.gapPx 8
            ]
        |> List.singleton
        |> Html.a
            [ Html.Attributes.href "#"
            , Html.Events.onClick args.onOpen
            , Html.Style.textDecorationNone
            , Html.Style.color "black"
            ]
    ]
        |> drawer
            ([ Html.Style.positionFixed
             , Html.Style.bottomPx 0
             , Html.Style.heightPx minDrawerHeight
             , Html.Style.transition "height 0.2s"
             ]
                ++ attrs
            )
