module View.Lab exposing (..)

import Config
import Dict
import Fish.Common exposing (Breed)
import Game exposing (Game)
import Html exposing (Html)
import Html.Attributes
import Layout
import View.Common


toHtml :
    { buyFish : Maybe Breed -> msg
    , buyTank : msg
    }
    -> Game
    -> Html msg
toHtml args game =
    [ View.Common.storage
        { onClick = \id -> Nothing
        , infos =
            \fishId ->
                [ View.Common.sizeInfo game fishId
                , View.Common.breedInfo game fishId
                ]
        }
        game
    , View.Common.money game.money
    , Layout.textButton []
        { label = "Buy Tank for " ++ String.fromInt Config.tankCost ++ " Money"
        , onPress = args.buyTank |> Just
        }
    , Html.text "Discovered Breeds"
    , game.breeds
        |> Dict.values
        |> List.map
            (\breed ->
                [ Fish.Common.new breed
                    |> View.Common.fishSprite []
                        { animationFrame = False }
                , Html.text breed.name
                , Layout.textButton []
                    { label = "Buy for " ++ String.fromInt Config.fishCost ++ " Money"
                    , onPress = Just breed |> args.buyFish |> Just
                    }
                ]
                    |> Layout.column [ Html.Attributes.style "width" "80px" ]
            )
        |> Layout.row [ Layout.gap 8 ]
    ]
        |> Layout.column [ Layout.gap 16, Layout.alignAtCenter, Layout.contentCentered ]
