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
    240


minDrawerHeight =
    66


drawer : List (Attribute msg) -> List (Html msg) -> Html msg
drawer attrs =
    Html.div
        ([ Html.Style.displayFlex
         , Html.Style.flexDirectionColumn
         , Html.Style.widthPx 400
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
        ([ Html.Style.displayFlex
         , Html.Style.flexDirectionColumn
         , Html.Style.backgroundColor "white"
         , Html.Style.borderRadius "16px"
         , Html.Style.widthPx 200
         , Html.Style.aspectRatio "2/3"
         , Html.Style.alignItemsCenter
         , Html.Style.justifyContentCenter
         , Html.Style.padding "16px"
         , Html.Style.boxSizingBorderBox
         ]
            ++ attrs
        )


detailCard : { bug : Bug, variant : Variant, caught : Int } -> Html msg
detailCard args =
    [ [ Layout.divText [ Html.Style.fontSizePx 64 ]
            (Bug.toString args.bug)
      , if args.caught >= 3 then
            [ "Found next to " |> Layout.divText []
            , (Bug.requirementsOf args.bug
                |> List.concatMap
                    (\( n, tile ) ->
                        tile
                            |> Maybe.map Object.toString
                            |> Maybe.withDefault "❌"
                            |> List.repeat n
                    )
                |> String.concat
              )
                |> Layout.divText
                    [ Html.Style.displayFlex
                    , Html.Style.justifyContentCenter
                    ]
            ]
                |> Html.div
                    [ Html.Style.displayFlex
                    , Html.Style.flexDirectionColumn
                    , Html.Style.gapPx 8
                    ]

        else
            Layout.divText []
                ("Catch " ++ (3 - args.caught |> String.fromInt) ++ " more to unlock")
      , "Caught " ++ String.fromInt args.caught |> Layout.divText []
      ]
        |> card [ Html.Style.gapPx 32 ]
    , case args.variant of
        Cute ->
            Layout.divWrapper [] Layout.none

        Royal ->
            View.Variant.royal
                [ Html.Style.borderRadius "16px"
                ]
    ]
        |> Html.div
            [ Html.Style.displayFlex
            , Html.Style.justifyContentCenter
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
            [ Html.Style.displayFlex
            , Html.Style.padding "8px 16px"
            , Html.Style.justifyContentCenter
            ]
    , (Bug.list
        |> List.map
            (\species ->
                [ case collectedBugs |> Collection.get species of
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
                , if Collection.count species collectedBugs < 3 then
                    String.fromInt (Collection.count species collectedBugs)
                        ++ " / 3"
                        |> Layout.divText
                            [ Html.Style.displayFlex
                            , Html.Style.justifyContentCenter
                            ]

                  else
                    (Bug.requirementsOf species
                        |> List.concatMap
                            (\( n, tile ) ->
                                tile
                                    |> Maybe.map Object.toString
                                    |> Maybe.withDefault "❌"
                                    |> List.repeat n
                            )
                        |> String.concat
                    )
                        |> Layout.divText
                            [ Html.Style.displayFlex
                            , Html.Style.justifyContentCenter
                            ]
                ]
                    |> Html.div
                        [ Html.Style.displayFlex
                        , Html.Style.flexDirectionColumn
                        , Html.Style.gapPx 8
                        , Html.Style.fontSizePx 12
                        ]
            )
      )
        |> Html.div
            [ Html.Style.displayGrid
            , Html.Style.gridTemplateColumns "repeat(6,1fr)"
            , Html.Style.paddingPx 16
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
                [ Html.Style.displayFlex
                , Html.Style.alignItemsCenter
                ]
      , (collectedBugs
            |> Collection.bugs
            |> List.length
            |> String.fromInt
        )
            |> Layout.divText
                [ Html.Style.displayFlex
                , Html.Attributes.style "font-size" "20px"
                , Html.Style.alignItemsCenter
                ]
      ]
        |> Html.div
            [ Html.Style.displayFlex
            , Html.Style.flexDirectionColumn
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
