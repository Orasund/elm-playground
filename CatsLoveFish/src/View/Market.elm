module View.Market exposing (..)

import Cat
import Config
import Dict
import Fish.Common exposing (Breed, FishId)
import Game exposing (Game, Order)
import Html exposing (Html)
import Html.Attributes
import Layout
import Pigment
import Set
import View.Common


toHtml :
    { sellFish : FishId -> msg
    , sellAllFishInStorage : msg
    , buyFish : Maybe Breed -> msg
    , buyTank : msg
    }
    -> Game
    -> Html msg
toHtml args game =
    [ storage
        { onClick = \id -> args.sellFish id
        , infos =
            \fishId ->
                [ priceInfo game fishId
                ]
        }
        game
    , View.Common.money game.money
    , [ View.Common.catSprite [] game.order.cat |> Layout.el []
      , orderInfo game.order
      ]
        |> Layout.row []
    ]
        |> Layout.column [ Layout.gap 16, Layout.alignAtCenter, Layout.contentCentered ]


orderInfo : Order -> Html msg
orderInfo order =
    let
        feature content =
            [ "+"
                ++ String.fromInt Config.featurePrice
                ++ " Money"
                |> Layout.text
                    [ Layout.fillPortion 1
                    , Layout.contentAtEnd
                    ]
            , content
                |> Layout.el [ Layout.fillPortion 2 ]
            ]
                |> Layout.row [ Layout.gap 16 ]
    in
    [ Html.text "New Order"
        |> Layout.el [ Html.Attributes.style "font-weight" "bold" ]
    , [ "5 Money"
            |> Layout.text
                [ Layout.fillPortion 1
                , Layout.contentAtEnd
                ]
      , "One fish that" |> Layout.text [ Layout.fillPortion 2 ]
      ]
        |> Layout.row [ Layout.gap 16 ]
    , [ Html.text "has a "
      , View.Common.pigmentCircle order.primary
      , " "
            ++ Pigment.name order.primary
            ++ " primary color"
            |> Html.text
      ]
        |> Html.span []
        |> feature
    , [ Html.text "has a "
      , View.Common.pigmentCircle order.secondary
      , " "
            ++ Pigment.name order.secondary
            ++ " secondary color"
            |> Html.text
      ]
        |> Html.span []
        |> feature
    ]
        |> Layout.column
            [ Html.Attributes.style "border" "1px solid black"
            , Html.Attributes.style "border-radius" "16px"
            , Html.Attributes.style "background-color" "white"
            , Html.Attributes.style "padding" "16px"
            ]


storage :
    { onClick : FishId -> msg
    , infos : FishId -> List (Html msg)
    }
    -> Game
    -> Html msg
storage args g =
    g.storage
        |> Set.toList
        |> List.filterMap
            (\fishId ->
                g.fish
                    |> Dict.get fishId
                    |> Maybe.map (Tuple.pair fishId)
            )
        |> List.map
            (\( fishId, fish ) ->
                args.infos fishId
                    |> (::)
                        ({ fish | size = 1 }
                            |> View.Common.fishSprite
                                (Layout.asButton
                                    { label = "Load"
                                    , onPress = Just (args.onClick fishId)
                                    }
                                )
                                { animationFrame = False }
                        )
                    |> Layout.column
                        [ Layout.alignAtCenter
                        , Html.Attributes.style "width" "100px"
                        , Html.Attributes.style "padding" "8px"
                        ]
            )
        |> Layout.row
            [ Layout.gap 8
            , Layout.alignAtEnd
            , Html.Attributes.style "height" "170px"
            ]


priceInfo : Game -> FishId -> Html msg
priceInfo game fishId =
    let
        order =
            game.order

        feature bool content =
            [ String.fromInt Config.featurePrice |> Layout.text [ Layout.fillPortion 1 ]
            , content |> Layout.el [ Layout.fillPortion 3 ]
            ]
                |> Layout.row
                    (if bool then
                        []

                     else
                        [ Html.Attributes.style "text-decoration" "line-through" ]
                    )
    in
    game.fish
        |> Dict.get fishId
        |> Maybe.map
            (\fish ->
                [ "Price" |> Layout.text [ Html.Attributes.style "font-weight" "bold" ]
                , [ String.fromInt Config.basePrice |> Layout.text [ Layout.fillPortion 1 ]
                  , "Base" |> Layout.text [ Layout.fillPortion 3 ]
                  ]
                    |> Layout.row []
                , [ View.Common.pigmentCircle order.primary
                  , " "
                        ++ Pigment.name order.primary
                        |> Html.text
                  ]
                    |> Html.span []
                    |> feature (order.primary == fish.primary)
                , [ View.Common.pigmentCircle order.secondary
                  , " "
                        ++ Pigment.name order.secondary
                        |> Html.text
                  ]
                    |> Html.span []
                    |> feature (order.secondary == fish.secondary)
                , Html.hr [ Html.Attributes.style "width" "100%" ] []
                , Game.price fish
                    { primary = game.order.primary, secondary = game.order.secondary }
                    |> (\price -> String.fromInt price ++ " Money")
                    |> Html.text
                ]
            )
        |> Maybe.withDefault []
        |> Layout.column
            [ Html.Attributes.style "background-color" "white"
            , Html.Attributes.style "border" "1px solid black"
            , Html.Attributes.style "border-radius" "8px"
            , Html.Attributes.style "padding" "8px"
            , Html.Attributes.style "font-size" "0.9em"
            , Html.Attributes.style "width" "100%"
            ]
