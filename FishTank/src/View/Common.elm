module View.Common exposing (..)

import Color exposing (Color)
import Config
import Dict
import Fish exposing (BitColor(..), Fish, FishId)
import Game exposing (Game, TankId)
import Html exposing (Attribute, Html)
import Html.Attributes
import Html.Keyed
import Image
import Image.Color
import Layout
import Rule exposing (Pattern(..))
import Set
import Svg.Path
import Svg.Writer
import Tank


game :
    { animationFrame : Bool
    , storeFish : FishId -> msg
    , loadFish : FishId -> msg
    , sellFish : FishId -> msg
    , tankId : TankId
    }
    -> Game
    -> Html msg
game args g =
    [ tank
        { animationFrame = args.animationFrame
        , storeFish = args.storeFish
        , tankId = args.tankId
        }
        g
    , g.storage
        |> Set.toList
        |> List.filterMap
            (\fishId ->
                g.fish
                    |> Dict.get fishId
                    |> Maybe.map (Tuple.pair fishId)
            )
        |> List.map
            (\( fishId, fish ) ->
                fish
                    |> fishWithInfos []
                        { load = args.loadFish fishId
                        , sell = args.sellFish fishId
                        }
            )
        |> Layout.row [ Layout.gap 16 ]
    ]
        |> Layout.column []


tank : { animationFrame : Bool, storeFish : FishId -> msg, tankId : TankId } -> Game -> Html msg
tank args g =
    [ g.tanks
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
                            |> fishSprite
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
                        (String.fromFloat (Config.gridSize * x) ++ "px")
                    , Html.Attributes.style "top"
                        (String.fromFloat (Config.gridSize * y) ++ "px")
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
            , Html.Attributes.style "height" (String.fromFloat (Config.gridSize * Config.tankHeight) ++ "px")
            , Html.Attributes.style "width" (String.fromFloat (Config.gridSize * Config.tankWidth) ++ "px")
            , Html.Attributes.style "border-radius" "0 0 16px 16px "
            , Html.Attributes.style "background-image" "linear-gradient(0deg,#A9C397 0%,#CEE5F2 100%)"
            ]


fishSprite : List (Attribute msg) -> { animationFrame : Bool } -> Fish -> Html msg
fishSprite attrs args f =
    Fish.toBitmap { animate = args.animationFrame } f.pattern
        |> List.map
            (List.map
                (\bitColor ->
                    case bitColor of
                        Black ->
                            Color.black

                        Primary ->
                            f.primary

                        Secondary ->
                            f.secondary

                        None ->
                            Color.fromRgba
                                { red = 0
                                , blue = 0
                                , green = 0
                                , alpha = 0
                                }
                )
            )
        |> Image.Color.fromList2d
        |> Image.toPngUrl
        |> (\url ->
                Html.img
                    ([ Html.Attributes.src url
                     , Html.Attributes.style "width" (String.fromInt (Config.fishMinSize + 2 * f.size) ++ "px")
                     , Html.Attributes.style "image-rendering" "pixelated"
                     ]
                        ++ attrs
                    )
                    []
           )


fishWithInfos : List (Attribute msg) -> { load : msg, sell : msg } -> Fish -> Html msg
fishWithInfos attrs args f =
    let
        ( rules1, rules2 ) =
            f.rules
                |> List.map
                    (\( b, p ) ->
                        ( b
                        , patternCircle
                            (if b then
                                f.secondary

                             else
                                f.primary
                            )
                            p
                        )
                    )
                |> List.partition Tuple.first
    in
    Layout.column attrs
        [ f
            |> fishSprite
                (Layout.asButton
                    { label = "Return to Tank"
                    , onPress = Just args.load
                    }
                )
                { animationFrame = False }
        , rules1
            |> List.map Tuple.second
            |> Layout.row []
        , rules2
            |> List.map Tuple.second
            |> Layout.row []
        , Layout.textButton []
            { label = "Sell"
            , onPress = Just args.sell
            }
        ]


patternCircle : Color -> Pattern -> Html msg
patternCircle color p =
    (case p of
        Horizontal ->
            Svg.Path.startAt ( 0, 5 )
                |> Svg.Path.drawLineTo ( 10, 5 )
                |> Svg.Path.end

        Vertical ->
            Svg.Path.startAt ( 5, 0 )
                |> Svg.Path.drawLineTo ( 5, 10 )
                |> Svg.Path.end

        BottomUp ->
            Svg.Path.startAt ( 0, 10 )
                |> Svg.Path.drawLineTo ( 10, 0 )
                |> Svg.Path.end

        TopDown ->
            Svg.Path.startAt ( 0, 0 )
                |> Svg.Path.drawLineTo ( 10, 10 )
                |> Svg.Path.end
    )
        |> Svg.Writer.path
        |> Svg.Writer.withStrokeWidth 3
        |> Svg.Writer.withStrokeColor (Color.toCssString color)
        |> List.singleton
        |> Svg.Writer.toHtml
            [ Html.Attributes.style "border-radius" "100%"
            , Html.Attributes.style "border" ("3px solid " ++ Color.toCssString color)
            ]
            { width = 10, height = 10 }
