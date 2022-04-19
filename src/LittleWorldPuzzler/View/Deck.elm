module LittleWorldPuzzler.View.Deck exposing (view, viewOne)

import Card
import Element exposing (Attribute, Element)
import Element.Font as Font
import LittleWorldPuzzler.Data.CellType as CellType exposing (CellType(..))
import LittleWorldPuzzler.Data.Deck as Deck exposing (Deck, Selected(..))
import LittleWorldPuzzler.View.Rule as RuleView


viewInactiveCard : Float -> Element msg -> Element msg
viewInactiveCard scale content =
    Element.el
        [ Element.width <| Element.px <| floor <| 120 * scale
        , Element.height <| Element.px <| floor <| 176 * scale
        , Element.alignTop
        , Element.padding <| floor <| 5 * scale
        ]
    <|
        content


viewCardList : Float -> Bool -> List CellType -> Element msg
viewCardList scale sort =
    List.map CellType.toString
        >> (if sort then
                List.sort

            else
                identity
           )
        >> List.map Element.text
        >> Element.wrappedRow
            [ Font.size <| floor <| 25 * scale
            , Element.spacing <| floor <| 5 * scale
            , Element.centerX
            ]


viewContent : Float -> CellType -> Element msg
viewContent scale cellType =
    Element.column
        [ Element.spacing <| floor <| 40 * scale
        , Element.centerX
        , Element.centerY
        ]
        [ Element.el [ Font.size <| floor <| 60 * scale, Element.centerX ] <|
            Element.text <|
                CellType.toString cellType
        , Element.column
            [ Font.size <| floor <| 11 * scale
            , Element.spacing <| floor <| 5 * scale
            , Element.centerX
            ]
          <|
            RuleView.view cellType
        ]


viewAttributes : Float -> List (Attribute msg)
viewAttributes scale =
    [ Element.centerX
    , Element.spaceEvenly

    --, Element.height <| Element.px <| floor <| 200 * scale
    , Element.width <| Element.fill
    ]


viewOne : Float -> Maybe CellType -> Element msg
viewOne scale maybeCellType =
    Element.el
        [ -- Element.height <| Element.px <| floor <| 200 * scale
          Element.centerX
        ]
    <|
        case maybeCellType of
            Just cellType ->
                Card.hand []
                    { width = 100 * scale
                    , dimensions = ( 120, 176 )
                    , cards =
                        List.singleton <|
                            Card.card
                                { attributes = []
                                , content = viewContent scale cellType
                                , onPress = Nothing
                                , selected = True
                                }
                    }

            Nothing ->
                Element.el
                    [ Font.size <| floor <| 40 * scale
                    , Font.family
                        [ Font.sansSerif ]
                    , Font.center
                    , Element.centerX
                    , Element.centerY
                    ]
                <|
                    Element.text "please select a card"


view : Float -> Bool -> Maybe (Selected -> msg) -> Maybe Selected -> Deck -> Element msg
view scale sort maybeSelectedMsg maybeSelected deck =
    Element.row (viewAttributes scale) <|
        [ viewInactiveCard scale <|
            Element.column
                [ Element.spacing <| floor <| 10 * scale
                , Element.centerX
                ]
                [ Element.el
                    [ Font.size <| floor <| 30 * scale
                    , Element.centerX
                    ]
                  <|
                    Element.text "📤"
                , viewCardList scale
                    sort
                    (deck
                        |> Deck.remaining
                        |> List.tail
                        |> Maybe.withDefault []
                    )
                ]
        , Card.hand
            [ Element.centerX

            --, Element.height <| Element.px <| floor <| 200 * scale
            ]
            { width = 250 * scale
            , dimensions = ( 120, 176 )
            , cards =
                List.concat
                    [ [ Card.card
                            { attributes = []
                            , content =
                                viewContent scale <|
                                    Deck.first deck
                            , onPress = maybeSelectedMsg |> Maybe.map (\fun -> fun First)
                            , selected = maybeSelected == Just First
                            }
                      ]
                    , case deck |> Deck.second of
                        Just cellType ->
                            [ Card.card
                                { attributes = []
                                , content = viewContent scale cellType
                                , onPress = maybeSelectedMsg |> Maybe.map (\fun -> fun Second)
                                , selected = maybeSelected == Just Second
                                }
                            ]

                        Nothing ->
                            []
                    ]
            }
        , viewInactiveCard scale <|
            Element.column
                [ Element.spacing <| floor <| 10 * scale
                , Element.centerX
                ]
                [ Element.el [ Font.size <| floor <| 30 * scale, Element.centerX ] <|
                    Element.text "🗑"
                , viewCardList scale sort (deck |> Deck.played)
                ]
        ]
