module LittleWorldPuzzler.View.Deck exposing (view)

import Browser
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Framework.Card as Card
import Framework.Color as Color
import Grid.Bordered as Grid exposing (Grid)
import Grid.Position as Position exposing (Position)
import Html exposing (Html)
import LittleWorldPuzzler.Automata.Neighborhood as Neighborhood
import LittleWorldPuzzler.Automata.Rule as Rule
import LittleWorldPuzzler.Data.CellType as CellType exposing (CellType(..))
import LittleWorldPuzzler.Data.Deck as Deck exposing (Deck, Selected(..))
import LittleWorldPuzzler.View.Button as Button
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


viewSelectable : Float -> msg -> Element msg -> Element msg
viewSelectable scale msg content =
    Button.view
        [ Element.width <| Element.px <| floor <| 120 * scale
        , Element.height <| Element.px <| floor <| 176 * scale
        , Element.alignBottom
        , Border.shadow
            { blur = 10
            , color = Element.rgba 0 0 0 0.05
            , offset = ( 0, 2 )
            , size = 1 * scale
            }
        , Border.rounded <| floor <| 4 * scale
        , Element.padding <| floor <| 5 * scale
        ]
    <|
        { onPress = Just msg
        , label = content
        }


viewSelected : Float -> Element msg -> Element msg
viewSelected scale content =
    Element.el
        [ Element.width <| Element.px <| floor <| 120 * scale
        , Element.height <| Element.px <| floor <| 176 * scale
        , Element.alignTop
        , Border.shadow
            { blur = 10
            , color = Element.rgba 0 0 0 0.05
            , offset = ( 0, 2 )
            , size = 1 * scale
            }
        , Border.width 1
        , Border.color <| Element.rgba255 219 219 219 1
        , Border.rounded <| floor <| 4 * scale
        , Element.padding <| floor <| 5 * scale
        , Background.color <| Element.rgb255 255 255 255
        ]
    <|
        content


viewCardList : Float -> List CellType -> Element msg
viewCardList scale =
    List.map CellType.toString
        >> List.sort
        >> List.map Element.text
        >> Element.wrappedRow
            [ Font.size <| floor <| 25 * scale
            , Element.spacing <| floor <| 5 * scale
            , Element.centerX
            ]


viewContent : Float -> CellType -> Element msg
viewContent scale cellType =
    Element.column [ Element.spacing <| floor <| 40 * scale, Element.centerX ]
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


view : Float -> (Selected -> msg) -> Maybe Selected -> Deck -> Element msg
view scale selectedMsg maybeSelected deck =
    Element.row
        [ Element.centerX
        , Element.spaceEvenly
        , Element.height <| Element.px <| floor <| 200 * scale
        , Element.width <| Element.fill
        ]
    <|
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
                    (deck
                        |> Deck.remaining
                        |> List.tail
                        |> Maybe.withDefault []
                    )
                ]
        , (if maybeSelected == Just First then
            viewSelected scale

           else
            viewSelectable scale <| selectedMsg First
          )
          <|
            viewContent scale <|
                Deck.first deck
        , case deck |> Deck.second of
            Just cellType ->
                (if maybeSelected == Just Second then
                    viewSelected scale

                 else
                    viewSelectable scale <| selectedMsg Second
                )
                <|
                    viewContent scale cellType

            Nothing ->
                viewInactiveCard scale <|
                    Element.text ""
        , viewInactiveCard scale <|
            Element.column
                [ Element.spacing <| floor <| 10 * scale
                , Element.centerX
                ]
                [ Element.el [ Font.size <| floor <| 30 * scale, Element.centerX ] <|
                    Element.text "🗑"
                , viewCardList scale (deck |> Deck.played)
                ]
        ]
