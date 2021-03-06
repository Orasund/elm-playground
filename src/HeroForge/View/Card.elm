module HeroForge.View.Card exposing (Msg(..), view)

import Card
import Color
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Framework.Card as Card
import Framework.Grid as Grid
import HeroForge.Data.Card as Card exposing (Card)
import HeroForge.Data.CardDetails as CardDetails
import HeroForge.Data.Effect as Effect
import HeroForge.Data.Game exposing (Direction(..))
import Html.Attributes as Attributes
import Html.Events.Extra.Touch as Touch
import List.Extra as List
import Tuple


type Msg
    = Selected Direction
    | Over (Maybe Direction)
    | Swiping (Maybe Float)


touchCoordinates : Touch.Event -> Float
touchCoordinates touchEvent =
    List.head touchEvent.changedTouches
        |> Maybe.map .clientPos
        |> Maybe.withDefault ( 0, 0 )
        |> Tuple.first


single : Card -> List (Element Msg)
single card =
    let
        { desc } =
            card |> CardDetails.getDetails

        { name, symbol } =
            card |> Card.toString
    in
    [ symbol ++ " " ++ name |> Element.text |> Element.el [ Font.bold ]
    , desc
        |> List.map (Element.text >> List.singleton >> Element.paragraph [])
        |> Element.column Grid.simple
    ]
        |> List.map
            (List.singleton
                >> Element.paragraph []
            )


view :
    { selected : Maybe Direction
    , card : Card
    , maybeNextCard : Maybe Card
    , showAnimation : Bool
    }
    -> Element Msg
view { selected, card, maybeNextCard, showAnimation } =
    let
        { left, right } =
            card |> CardDetails.getDetails

        { color } =
            card |> Card.toString
    in
    Element.column
        (Grid.simple
            ++ [ Element.htmlAttribute <|
                    Touch.onMove (touchCoordinates >> Just >> Swiping)
               , Element.htmlAttribute <|
                    Touch.onEnd (always (Swiping Nothing))
               ]
        )
        [ Element.row Grid.spaceEvenly
            [ Input.button
                [ Element.width <| Element.fill
                , Element.padding 10
                , Element.height <| Element.fill
                , Events.onMouseUp <| Selected Left
                , Events.onMouseEnter <| Over (Just Left)
                , Events.onMouseLeave <| Over Nothing
                , Element.focused <|
                    [ Border.shadow
                        { offset = ( 0, 0 )
                        , size = 0
                        , blur = 0
                        , color = Element.rgb255 0 0 0
                        }
                    ]
                ]
                { onPress = Nothing
                , label = Element.none
                }
            , card
                |> single
                |> Element.textColumn
                    (Grid.simple
                        ++ Card.simple
                        ++ [ Element.width <| Element.px <| 200
                           , Element.height <| Element.px <| 300
                           ]
                        ++ (if showAnimation then
                                [ Element.htmlAttribute <|
                                    Attributes.style "transition" "transform 1s"
                                , Element.htmlAttribute <|
                                    Attributes.style "transition-timing-function" "ease"
                                ]

                            else
                                [ Element.htmlAttribute <|
                                    Attributes.style "transition" "transform 0s"
                                , Element.htmlAttribute <|
                                    Attributes.style "transition-timing-function" "ease"
                                ]
                           )
                        ++ (color
                                |> Color.toRgba
                                |> Element.fromRgb
                                |> Background.color
                                |> List.singleton
                           )
                        ++ (case selected of
                                Just Left ->
                                    [ Element.moveLeft 50 ]

                                Just Right ->
                                    [ Element.moveRight 50 ]

                                Nothing ->
                                    []
                           )
                    )
                |> Element.el
                    [ Element.behindContent <|
                        case maybeNextCard of
                            Just nextCard ->
                                let
                                    nextColor =
                                        nextCard |> Card.toString |> .color
                                in
                                nextCard
                                    |> single
                                    |> Element.textColumn
                                        (Grid.simple
                                            ++ Card.simple
                                            ++ [ Element.width <| Element.px <| 200
                                               , Element.height <| Element.px <| 300
                                               ]
                                            ++ (nextColor
                                                    |> Color.toRgba
                                                    |> Element.fromRgb
                                                    |> Background.color
                                                    |> List.singleton
                                               )
                                        )

                            Nothing ->
                                Element.none
                    ]
            , Input.button
                [ Element.width <| Element.fill
                , Element.height <| Element.fill
                , Events.onMouseUp <| Selected Right
                , Events.onMouseEnter <| Over (Just Right)
                , Events.onMouseLeave <| Over Nothing
                , Element.focused <|
                    [ Border.shadow
                        { offset = ( 0, 0 )
                        , size = 0
                        , blur = 0
                        , color = Element.rgb255 0 0 0
                        }
                    ]
                ]
                { onPress = Nothing
                , label =
                    Element.none
                }
            ]
        , Element.row Grid.spaceEvenly
            ((case left of
                Just ( title, effects ) ->
                    [ Input.button
                        [ Element.width <| Element.fill
                        , Element.padding 10
                        , Element.height <| Element.fill
                        , Events.onMouseEnter <| Over (Just Left)
                        , Events.onMouseLeave <| Over Nothing
                        ]
                        { onPress = Just <| Selected Left
                        , label =
                            [ [ "<- "
                                    ++ title
                                    |> Element.text
                                    |> Element.el
                                        ([ Font.bold
                                         , Element.width <| Element.fill
                                         ]
                                            ++ (effects
                                                    |> List.filterMap (Effect.toString >> Tuple.second)
                                                    |> List.last
                                                    |> Maybe.map
                                                        (Color.toRgba
                                                            >> Element.fromRgb
                                                            >> Font.color
                                                            >> List.singleton
                                                        )
                                                    |> Maybe.withDefault []
                                               )
                                        )
                              ]
                            , effects
                                |> List.map
                                    (Effect.toString
                                        >> (\( text, maybeColor ) ->
                                                text
                                                    |> Element.text
                                                    |> Element.el
                                                        ([ Element.width <| Element.fill
                                                         , Font.size 11
                                                         ]
                                                            ++ (case maybeColor of
                                                                    Just c ->
                                                                        [ c
                                                                            |> Color.toRgba
                                                                            |> Element.fromRgb
                                                                            |> Font.color
                                                                        , Font.bold
                                                                        ]

                                                                    Nothing ->
                                                                        []
                                                               )
                                                        )
                                           )
                                    )
                            ]
                                |> List.concat
                                |> Element.column
                                    (Grid.compact
                                        ++ [ Element.width <| Element.shrink
                                           , Element.alignRight
                                           , Font.alignRight
                                           ]
                                    )
                        }
                    ]

                Nothing ->
                    [ Element.el
                        [ Element.width <| Element.fill
                        , Element.padding 10
                        , Element.height <| Element.fill
                        ]
                      <|
                        Element.none
                    ]
             )
                ++ [ Input.button
                        [ Font.alignLeft
                        , Element.width <| Element.fill
                        , Element.padding 10
                        , Element.height <| Element.fill
                        , Events.onMouseEnter <| Over (Just Right)
                        , Events.onMouseLeave <| Over Nothing
                        ]
                        { onPress = Just <| Selected Right
                        , label =
                            [ [ (right |> Tuple.first)
                                    ++ " ->"
                                    |> Element.text
                                    |> Element.el
                                        (Font.bold
                                            :: (right
                                                    |> Tuple.second
                                                    |> List.filterMap (Effect.toString >> Tuple.second)
                                                    |> List.last
                                                    |> Maybe.map
                                                        (Color.toRgba
                                                            >> Element.fromRgb
                                                            >> Font.color
                                                            >> List.singleton
                                                        )
                                                    |> Maybe.withDefault []
                                               )
                                        )
                              ]
                            , right
                                |> Tuple.second
                                |> List.map
                                    (Effect.toString
                                        >> (\( text, maybeColor ) ->
                                                text
                                                    |> Element.text
                                                    |> List.singleton
                                                    |> Element.paragraph
                                                        ([ Font.size 11
                                                         , Element.spacing 0
                                                         ]
                                                            ++ (case maybeColor of
                                                                    Just c ->
                                                                        [ c
                                                                            |> Color.toRgba
                                                                            |> Element.fromRgb
                                                                            |> Font.color
                                                                        , Font.bold
                                                                        ]

                                                                    Nothing ->
                                                                        []
                                                               )
                                                        )
                                           )
                                    )
                            ]
                                |> List.concat
                                |> Element.column Grid.compact
                        }
                   ]
            )
        ]
