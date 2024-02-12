module View.Collection exposing (closedCollection, detailCard, openCollection)

import BugSpecies exposing (BugSpecies)
import Html exposing (Attribute, Html)
import Html.Attributes
import Html.Events
import Html.Style
import Layout
import Object
import Set.Any as AnySet exposing (AnySet)
import View.Bubble


maxDrawerHeight =
    200


drawer : List (Attribute msg) -> List (Html msg) -> Html msg
drawer attrs =
    Layout.column
        ([ Html.Attributes.style "width" "352px"
         , Html.Attributes.style "background-color" "white"
         , Html.Attributes.style "border-top-left-radius" "8px"
         , Html.Attributes.style "border-top-right-radius" "8px"
         , Html.Style.boxSizingBorderBox
         ]
            ++ attrs
        )


card : List (Attribute msg) -> List (Html msg) -> Html msg
card attrs =
    Layout.column
        ([ Html.Attributes.style "background-color" "white"
         , Html.Attributes.style "border-radius" "16px"
         , Html.Attributes.style "width" "200px"
         , Html.Attributes.style "aspect-ratio" "2/3"
         ]
            ++ attrs
        )


detailCard : BugSpecies -> Html msg
detailCard bug =
    [ Layout.text [ Html.Attributes.style "font-size" "64px" ]
        (BugSpecies.toString bug)
    , [ "Found next to " |> Layout.text []
      , (BugSpecies.requirementsOf bug
            |> List.concatMap
                (\( n, tile ) ->
                    tile
                        |> Maybe.map Object.toString
                        |> Maybe.withDefault "❌"
                        |> List.repeat n
                )
            |> String.concat
        )
            |> Layout.text [ Html.Style.justifyContentCenter ]
      ]
        |> Layout.column [ Html.Style.gap "8px" ]
    ]
        |> card
            (Layout.centered
                ++ [ Html.Style.positionAbsolute
                   , "calc(50% - " ++ String.fromFloat (maxDrawerHeight / 2) ++ "px)" |> Html.Style.top
                   , Html.Style.left "50%"
                   , Html.Attributes.style "transform" "translate(-50%,-50%)"
                   , Html.Style.gap "32px"
                   ]
            )


openCollection :
    List (Attribute msg)
    ->
        { selected : Maybe BugSpecies
        , onSelect : BugSpecies -> msg
        }
    -> AnySet String BugSpecies
    -> Html msg
openCollection attrs args collectedBugs =
    [ "Your collection:"
        |> Html.text
        |> Layout.el
            [ Html.Attributes.style "padding" "8px 16px"
            , Html.Attributes.style "height" "48px"
            , Html.Style.alignItemsCenter
            ]
    , (BugSpecies.list
        |> List.map
            (\species ->
                if collectedBugs |> AnySet.member species then
                    BugSpecies.toString species
                        |> View.Bubble.button []
                            { label = "View details of " ++ BugSpecies.toString species
                            , onPress = Just (args.onSelect species)
                            }

                else
                    BugSpecies.toString species
                        |> View.Bubble.unkown []
            )
      )
        |> Layout.row
            [ Html.Attributes.style "padding" "16px"
            , Html.Attributes.style "font-size" "20px"
            , Html.Style.alignItemsCenter
            , Layout.gap 16
            ]
    ]
        |> drawer
            ([ Html.Attributes.style "position" "fixed"
             , Html.Attributes.style "bottom" "0"
             , Html.Attributes.style "height" (String.fromFloat maxDrawerHeight ++ "px")
             , Html.Attributes.style "transition" "height 0.2s"
             ]
                ++ attrs
            )


closedCollection :
    List (Attribute msg)
    -> { onOpen : msg }
    -> AnySet String BugSpecies
    -> Html msg
closedCollection attrs args collectedBugs =
    [ [ "Your collection:"
            |> Layout.text
                [ Html.Attributes.style "padding" "8px 16px"
                , Html.Style.alignItemsCenter
                ]
      , (collectedBugs
            |> AnySet.toList
            |> List.map BugSpecies.toString
            |> String.concat
        )
            |> Html.text
            |> Layout.el
                [ Html.Attributes.style "padding" "8px 16px"
                , Html.Attributes.style "font-size" "20px"
                , Html.Style.alignItemsCenter
                ]
      ]
        |> Layout.row
            [ Html.Attributes.style "height" "48px"
            , Html.Style.alignItemsCenter
            , Layout.contentWithSpaceBetween
            ]
        |> List.singleton
        |> Html.a
            [ Html.Attributes.href "#"
            , Html.Events.onClick args.onOpen
            , Html.Attributes.style "text-decoration" "none"
            , Html.Attributes.style "color" "black"
            ]
    ]
        |> drawer
            ([ Html.Attributes.style "position" "fixed"
             , Html.Attributes.style "bottom" "0"
             , Html.Attributes.style "height" "48px"
             , Html.Attributes.style "transition" "height 0.2s"
             ]
                ++ attrs
            )
