module Katakomben.View.Card exposing (view)

import Card
import Element exposing (Element)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Framework.Card as Card
import Framework.Grid as Grid
import Katakomben.Data.Card as Card exposing (Card)
import Katakomben.Data.CardDetails as CardDetails
import Katakomben.Data.Effect as Effect
import Katakomben.Data.Game exposing (Direction(..))


view :
    Card
    -> Element Direction
view card =
    let
        { name, left, right, desc, color } =
            card |> CardDetails.getDetails
    in
    Element.column Grid.simple
        [ Element.row Grid.spaceEvenly
            [ Input.button
                [ Element.width <| Element.fill
                , Element.padding 10
                , Element.height <| Element.fill
                ]
                { onPress = Just Left
                , label = Element.none
                }
            , [ name |> Element.text |> Element.el [ Font.bold ]
              , desc |> Element.text |> Element.el [ Font.italic ]
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
                        ++ (color |> List.map (Element.mapAttribute never))
                    )
            , Input.button
                [ Element.width <| Element.fill
                , Element.height <| Element.fill
                ]
                { onPress = Just Right
                , label =
                    Element.none
                }
            ]
        , Element.row Grid.spaceEvenly
            [ Input.button
                [ Element.width <| Element.fill
                , Element.padding 10
                , Element.height <| Element.fill
                ]
                { onPress = Just Left
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
                ]
                { onPress = Just Right
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
                                            |> Element.el
                                                ([ Font.size 11 ]
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
