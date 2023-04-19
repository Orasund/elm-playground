module View.Overlay exposing (..)

import Action exposing (Action(..))
import Color
import Dict
import Fish
import Game exposing (Game)
import Html exposing (Html)
import Html.Attributes
import Layout
import Set
import View.Common


toHtml : { onSubmit : msg } -> Game -> Maybe Action -> Html msg
toHtml args game maybeAction =
    case maybeAction of
        Just (NewBreed breedId) ->
            let
                fish =
                    game.breeds
                        |> Dict.get breedId
                        |> Maybe.map .pattern
                        |> Maybe.withDefault Set.empty
                        |> Set.toList
                        |> (\pattern ->
                                Fish.new
                                    { pattern = pattern
                                    , primary = Color.white
                                    , secondary = Color.black
                                    }
                           )
            in
            [ Html.text "new breed found"
            , fish
                |> View.Common.fishSprite [] { animationFrame = False }
            , Layout.textButton []
                { onPress = args.onSubmit |> Just
                , label = "Submit"
                }
            ]
                |> Layout.column
                    [ Html.Attributes.style "width" "300px"
                    , Html.Attributes.style "height" "300px"
                    , Html.Attributes.style "background-color" "white"
                    , Html.Attributes.style "border-radius" "16px"
                    , Html.Attributes.style "padding" "16px"
                    , Html.Attributes.style "position" "relative"
                    ]
                |> Layout.el
                    ([ Html.Attributes.style "position" "absolute"
                     , Html.Attributes.style "top" "0"
                     , Html.Attributes.style "left" "0"
                     , Html.Attributes.style "width" "100%"
                     , Html.Attributes.style "height" "100%"
                     ]
                        ++ Layout.centered
                    )

        Nothing ->
            Layout.none
