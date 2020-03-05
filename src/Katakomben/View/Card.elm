module Katakomben.View.Card exposing (Msg(..), view)

import Card
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Framework.Card as Card
import Framework.Grid as Grid
import Html.Attributes as Attributes
import Html.Events
import Html.Events.Extra.Touch as Touch
import Katakomben.Data.Card as Card exposing (Card)
import Katakomben.Data.CardDetails as CardDetails
import Katakomben.Data.Effect as Effect
import Katakomben.Data.Game exposing (Direction(..))
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


view :
    { selected : Maybe Direction
    , card : Card
    , maybeNextCard : Maybe Card
    , showAnimation : Bool
    }
    -> Element Msg
view { selected, card, maybeNextCard, showAnimation } =
    let
        { name, left, right, desc, color } =
            card |> CardDetails.getDetails
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
            , [ name |> Element.text |> Element.el [ Font.bold ]
              , desc |> Element.text
              ]
                |> List.map
                    (List.singleton
                        >> Element.paragraph []
                    )
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
                        ++ (color |> List.map (Element.mapAttribute never))
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
                                    nextDetails =
                                        nextCard |> CardDetails.getDetails
                                in
                                [ nextDetails.name |> Element.text
                                ]
                                    |> List.map
                                        (List.singleton
                                            >> Element.paragraph []
                                        )
                                    |> Element.textColumn
                                        (Grid.simple
                                            ++ Card.simple
                                            ++ [ Element.width <| Element.px <| 200
                                               , Element.height <| Element.px <| 300
                                               ]
                                            ++ (nextDetails.color |> List.map (Element.mapAttribute never))
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
                            ++ (left |> Tuple.first)
                            |> Element.text
                            |> Element.el
                                [ Font.bold
                                , Element.width <| Element.fill
                                ]
                      ]
                    , left
                        |> Tuple.second
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
                                                                [ Font.color c
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
            , Input.button
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
                            |> Element.el [ Font.bold ]
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
                                                                [ Font.color c
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
        ]
