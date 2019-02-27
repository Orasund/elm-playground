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


viewInactiveCard : Element msg -> Element msg
viewInactiveCard content =
    Element.el
        [ Element.width <| Element.px <| 120
        , Element.height <| Element.px <| 176
        , Element.alignBottom
        , Element.padding 20
        ]
    <|
        content


viewSelectable : msg -> Element msg -> Element msg
viewSelectable msg content =
    Element.el
        [ Element.width <| Element.px <| 120
        , Element.height <| Element.px <| 176
        , Element.alignBottom
        , Border.shadow { blur = 10, color = Element.rgba 0 0 0 0.05, offset = ( 0, 2 ), size = 1 }
        , Border.width 1
        , Border.color <| Element.rgba255 219 219 219 1
        , Border.rounded 4
        , Element.padding 20
        , Events.onClick msg
        , Background.color <| Element.rgb255 255 255 255
        ]
    <|
        content


viewSelected : Element msg -> Element msg
viewSelected content =
    Element.el
        [ Element.width <| Element.px <| 120
        , Element.height <| Element.px <| 176
        , Element.alignTop
        , Border.shadow { blur = 10, color = Element.rgba 0 0 0 0.05, offset = ( 0, 2 ), size = 1 }
        , Border.width 1
        , Border.color <| Element.rgba255 219 219 219 1
        , Border.rounded 4
        , Element.padding 20
        , Background.color <| Element.rgb255 255 255 255
        ]
    <|
        content


viewRules : CellType -> List (Element msg)
viewRules =
    Rule.rules
        >> List.map
            (\{ from, to, neighbors } ->
                Element.text <|
                    (from |> Maybe.map CellType.toString |> Maybe.withDefault "⭕")
                        ++ "➕"
                        ++ Neighborhood.toString neighbors
                        ++ "➡"
                        ++ (to |> Maybe.map CellType.toString |> Maybe.withDefault "⭕")
            )


viewCardList : List CellType -> Element msg
viewCardList =
    List.map CellType.toString
        >> List.sort
        >> List.map Element.text
        >> Element.wrappedRow [ Font.size 18, Element.spacing 5, Element.centerX ]


viewContent : CellType -> Element msg
viewContent cellType =
    Element.column [ Element.spacing 40, Element.centerX ]
        [ Element.el [ Font.size 60, Element.centerX ] <|
            Element.text <|
                CellType.toString cellType
        , Element.column [ Font.size 8, Element.spacing 5, Element.centerX ] <|
            viewRules cellType
        ]


view : (Selected -> msg) -> Maybe Selected -> Deck -> Element msg
view selectedMsg maybeSelected deck =
    Element.row
        [ Element.centerX
        , Element.spacing 10
        , Element.height <| Element.px <| 200
        ]
    <|
        [ viewInactiveCard <|
            Element.column [ Element.spacing 10, Element.centerX ]
                [ Element.el [ Font.size 30, Element.centerX ] <|
                    Element.text "📤"
                , viewCardList
                    (deck
                        |> Deck.remaining
                        |> List.tail
                        |> Maybe.withDefault []
                    )
                ]
        , (if maybeSelected == Just First then
            viewSelected

           else
            viewSelectable <| selectedMsg First
          )
          <|
            viewContent <|
                Deck.first deck
        , case deck |> Deck.second of
            Just cellType ->
                (if maybeSelected == Just Second then
                    viewSelected

                 else
                    viewSelectable <| selectedMsg Second
                )
                <|
                    viewContent cellType

            Nothing ->
                viewInactiveCard <|
                    Element.text ""
        , viewInactiveCard <|
            Element.column [ Element.spacing 10, Element.centerX ]
                [ Element.el [ Font.size 30, Element.centerX ] <|
                    Element.text "🗑"
                , viewCardList (deck |> Deck.played)
                ]
        ]
