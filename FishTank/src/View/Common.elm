module View.Common exposing (..)

import Cat exposing (Cat)
import Color exposing (Color)
import Config
import Dict
import Fish
import Fish.Common exposing (BitColor(..), Fish, FishId)
import Game exposing (Game)
import Html exposing (Attribute, Html)
import Html.Attributes
import Image
import Image.Color
import Layout
import Pigment exposing (Pigment)
import Rule exposing (Pattern(..))
import Set
import Svg.Path
import Svg.Writer


storage :
    { onClick : FishId -> Maybe msg
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
                            |> fishSprite
                                (Layout.asButton
                                    { label = "Load"
                                    , onPress = args.onClick fishId
                                    }
                                )
                                { animationFrame = False }
                        )
                    |> Layout.column (Layout.centered ++ [ Html.Attributes.style "width" "120px" ])
            )
        |> Layout.row
            [ Layout.gap 8
            , Layout.alignAtEnd
            , Layout.contentCentered
            , Html.Attributes.style "height" "120px"
            ]


sizeInfo : Game -> FishId -> Html msg
sizeInfo game fishId =
    game.fish
        |> Dict.get fishId
        |> Maybe.map .size
        |> Maybe.withDefault 0
        |> (\size -> "Size: " ++ String.fromInt size)
        |> Layout.text []


breedInfo : Game -> FishId -> Html msg
breedInfo game fishId =
    let
        primary =
            game.fish
                |> Dict.get fishId
                |> Maybe.map
                    (\fish ->
                        pigmentCircle fish.primary
                    )
                |> Maybe.withDefault Layout.none

        secondary =
            game.fish
                |> Dict.get fishId
                |> Maybe.map
                    (\fish ->
                        pigmentCircle fish.secondary
                    )
                |> Maybe.withDefault Layout.none

        breedName =
            game.assignedBreed
                |> Dict.get fishId
                |> Maybe.andThen
                    (\breedId ->
                        game.breeds |> Dict.get breedId
                    )
                |> Maybe.map .name
                |> Maybe.withDefault "Common Goldfish"
    in
    [ [ primary
      , secondary
      ]
        |> Layout.row [ Layout.gap 8 ]
    , breedName
        |> Layout.text
            [ Html.Attributes.style "font-size" "0.9rem"
            , Html.Attributes.style "width" "60px"
            ]
    ]
        |> Layout.column [ Layout.noWrap, Layout.contentWithSpaceBetween ]


money : Int -> Html msg
money int =
    String.fromInt int
        |> Layout.text
            [ Html.Attributes.style "font-size" "3rem"
            ]


catSprite : List (Attribute msg) -> Cat -> Html msg
catSprite attrs f =
    let
        width =
            Config.catSize
    in
    Cat.toBitmap f.pattern
        |> List.map
            (List.map
                (\bitColor ->
                    case bitColor of
                        Black ->
                            Color.black

                        Primary ->
                            Pigment.color True f.primary

                        Secondary ->
                            Pigment.color False f.secondary

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
                     , Html.Attributes.style "width" (String.fromFloat width ++ "px")
                     , Html.Attributes.style "height" (String.fromFloat width ++ "px")
                     , Html.Attributes.style "image-rendering" "pixelated"
                     ]
                        ++ attrs
                    )
                    []
           )


fishSprite : List (Attribute msg) -> { animationFrame : Bool } -> Fish -> Html msg
fishSprite attrs args f =
    let
        width =
            Config.fishMinSize * 2 ^ ((toFloat f.size - 1) / 4)
    in
    Fish.toBitmap { animate = args.animationFrame } f.pattern
        |> List.map
            (List.map
                (\bitColor ->
                    case bitColor of
                        Black ->
                            Color.black

                        Primary ->
                            Pigment.color True f.primary

                        Secondary ->
                            Pigment.color False f.secondary

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
                     , Html.Attributes.style "width" (String.fromFloat width ++ "px")
                     , Html.Attributes.style "image-rendering" "pixelated"
                     ]
                        ++ attrs
                    )
                    []
           )


fishInfo : Fish -> Html msg
fishInfo fish =
    let
        ( rules1, rules2 ) =
            fish.rules
                |> List.map
                    (\( b, p ) ->
                        ( b
                        , patternCircle
                            (if b then
                                Pigment.color False fish.secondary

                             else
                                Pigment.color True fish.primary
                            )
                            p
                        )
                    )
                |> List.partition Tuple.first
    in
    [ rules1
        |> List.map Tuple.second
        |> Layout.row []
    , rules2
        |> List.map Tuple.second
        |> Layout.row []
    ]
        |> Layout.column []


pigmentCircle : Pigment -> Html msg
pigmentCircle pigment =
    pigment
        |> Pigment.name
        |> String.left 1
        |> Layout.text [ Html.Attributes.style "font-size" "0.5em" ]
        |> Layout.el
            ([ pigment
                |> Pigment.color False
                |> Color.toCssString
                |> Html.Attributes.style "background-color"
             , Html.Attributes.style "width" "0.9em"
             , Html.Attributes.style "height" "0.9em"
             , Html.Attributes.style "border-radius" "100%"
             , Html.Attributes.style "border" "1px solid black"
             , Html.Attributes.style "display" "inline-flex"
             ]
                ++ Layout.centered
            )


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
            , Layout.asEl
            ]
            { width = 10, height = 10 }
