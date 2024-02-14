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
         , Html.Style.alignItemsCenter
         , Html.Style.justifyContentCenter
         ]
            ++ attrs
        )


detailCard : (Bug,Variant) -> Html msg
detailCard (bug ,variant) =
    [[ Layout.text [ Html.Attributes.style "font-size" "64px" ]
        (Bug.toString bug)
    , [ "Found next to " |> Layout.text []
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
            |> Layout.text [ Html.Style.justifyContentCenter ]
      ]
        |> Layout.column [ Html.Style.gap "8px" ]
    ]
        |> card [ Html.Style.gap "32px" ]
        , case variant of
            Cute -> Layout.el [] Layout.none
            Royal -> 
                View.Variant.royal 
                  [ Html.Attributes.style "border-radius" "16px"
                  ]
        ]
        |> Html.div
            (Layout.centered
                ++ [ Html.Style.positionAbsolute
                   , "calc(50% - " ++ String.fromFloat (maxDrawerHeight / 2) ++ "px)" |> Html.Style.top
                   , Html.Style.left "50%"
                   , Html.Attributes.style "transform" "translate(-50%,-50%)"
                   
                   ]
            )


openCollection :
    List (Attribute msg)
    ->
        { onSelect : (Bug,Variant) -> msg
        }
    -> Collection
    -> Html msg
openCollection attrs args collectedBugs =
    [ "Your collection:"
        |> Html.text
        |> Layout.el
            [ Html.Attributes.style "padding" "8px 16px"
            , Html.Attributes.style "height" "48px"
            , Html.Style.alignItemsCenter
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
                                    , onPress = Just (args.onSelect (species,
                                    if List.member Royal list then Royal else Cute
                                    ))
                                    }
                                    (Bug.toString species)
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
    -> Collection
    -> Html msg
closedCollection attrs args collectedBugs =
    [ [ "Your collection:"
            |> Layout.text
                [ Html.Attributes.style "padding" "8px 16px"
                , Html.Style.alignItemsCenter
                ]
      , (collectedBugs
            |> Collection.bugs
            |> List.map Bug.toString
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
