module View.Tank exposing (..)

import Config
import Dict
import Fish exposing (FishId)
import Game exposing (Game, TankId)
import Html exposing (Html)
import Html.Attributes
import Html.Keyed
import Layout
import Svg.Path
import Svg.Writer
import Tank
import View.Common


toHtml : { animationFrame : Bool, storeFish : FishId -> msg, tankId : TankId } -> Game -> Html msg
toHtml args g =
    [ ( "waves"
      , waves
            { offset =
                if args.animationFrame then
                    pi / 2

                else
                    pi * 3 / 2
            }
      )
        |> List.singleton
    , g.tanks
        |> Dict.get args.tankId
        |> Maybe.map Tank.fishIds
        |> Maybe.withDefault []
        |> List.filterMap
            (\fishId ->
                g.tanks
                    |> Dict.get args.tankId
                    |> Maybe.andThen (Tank.getFishLocation fishId)
                    |> Maybe.map (Tuple.pair fishId)
            )
        |> List.filterMap
            (\( fishId, ( x, y ) ) ->
                Maybe.map2
                    (\fish direction ->
                        fish
                            |> View.Common.fishSprite
                                ([ Html.Attributes.style "position" "absolute"
                                 , Html.Attributes.style "left"
                                    (String.fromFloat (Config.gridSize * x) ++ "px")
                                 , Html.Attributes.style "top"
                                    (String.fromFloat (Config.gridSize * y) ++ "px")
                                 , Html.Attributes.style "transition" "left 1s linear, top 1s linear"
                                 , (if direction < -pi / 2 then
                                        "scale( -1,1)"

                                    else if direction < pi / 2 then
                                        "scale(1,1)"

                                    else
                                        "scale(-1,1)"
                                   )
                                    |> Html.Attributes.style "transform"
                                 ]
                                    ++ Layout.asButton
                                        { onPress = args.storeFish fishId |> Just
                                        , label = "Store Fish"
                                        }
                                )
                                { animationFrame = args.animationFrame }
                            |> Tuple.pair ("fish" ++ String.fromInt fishId)
                    )
                    (Dict.get fishId g.fish)
                    (Dict.get fishId g.directions)
            )
    , g.tanks
        |> Dict.get args.tankId
        |> Maybe.map Tank.getFoods
        |> Maybe.withDefault Dict.empty
        |> Dict.toList
        |> List.map
            (\( foodId, ( x, y ) ) ->
                Html.div
                    [ Html.Attributes.style "position" "absolute"
                    , Html.Attributes.style "left"
                        (String.fromFloat (Config.gridSize * (x + 0.5)) ++ "px")
                    , Html.Attributes.style "top"
                        (String.fromFloat (Config.gridSize * (y + 0.5)) ++ "px")
                    , Html.Attributes.style "transition" "left 1s linear, top 1s linear"
                    , Html.Attributes.style "background-color" "yellow"
                    , Html.Attributes.style "height" "10px"
                    , Html.Attributes.style "width" "10px"
                    ]
                    []
                    |> Tuple.pair ("food" ++ String.fromInt foodId)
            )
    ]
        |> List.concat
        |> Html.Keyed.node "div"
            [ Html.Attributes.style "position" "relative"
            , Html.Attributes.style "height" (String.fromFloat (Config.gridSize * (Config.tankHeight + 1)) ++ "px")
            , Html.Attributes.style "width" (String.fromFloat (Config.gridSize * (Config.tankWidth + 1)) ++ "px")
            , Html.Attributes.style "border-radius" "0 0 16px 16px "
            , Html.Attributes.style "overflow" "hidden"
            ]


waves : { offset : Float } -> Html msg
waves args =
    let
        width =
            Config.gridSize * (Config.tankWidth + 1)

        height =
            Config.gridSize * (Config.tankHeight + 1)

        pointAmount =
            30

        waveAmplitute =
            2

        waveHeight i =
            waveAmplitute + waveAmplitute * sin (args.offset + (toFloat i / pointAmount) * pi * 2)
    in
    [ [ Svg.Writer.custom "stop" []
            |> Svg.Writer.withCustomAttribute "offset" "0%"
            |> Svg.Writer.withCustomAttribute "style" "stop-color:#CEE5F2"
      , Svg.Writer.custom "stop" []
            |> Svg.Writer.withCustomAttribute "offset" "100%"
            |> Svg.Writer.withCustomAttribute "style" "stop-color:#A9C397"
      ]
        |> Svg.Writer.custom "linearGradient"
        |> Svg.Writer.withCustomAttribute "id" "grad"
        |> Svg.Writer.withCustomAttribute "x1" "0%"
        |> Svg.Writer.withCustomAttribute "x2" "0%"
        |> Svg.Writer.withCustomAttribute "y1" "0%"
        |> Svg.Writer.withCustomAttribute "y2" "100%"
    , List.range 1 pointAmount
        |> List.foldl
            (\i ->
                Svg.Path.drawLineTo
                    ( width * toFloat i / pointAmount
                    , waveHeight i
                    )
            )
            (Svg.Path.startAt ( 0, waveHeight 0 ))
        |> Svg.Path.drawLineTo ( width, 0 )
        |> Svg.Path.drawLineTo ( width, height )
        |> Svg.Path.drawLineTo ( 0, height )
        |> Svg.Path.endClosed
        |> Svg.Writer.path
        |> Svg.Writer.withFillColor "url(#grad)"
    ]
        |> Svg.Writer.toHtml []
            { width = width
            , height = height
            }
