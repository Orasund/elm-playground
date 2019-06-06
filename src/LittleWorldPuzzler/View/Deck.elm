module LittleWorldPuzzler.View.Deck exposing (view, viewOne)

import Element exposing (Attribute, Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
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


viewSelectable : Float -> Selected -> Maybe (Selected -> msg) -> Element msg -> Element msg
viewSelectable scale selected maybeMsg content =
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
        { onPress = maybeMsg |> Maybe.map (\fun -> fun selected)
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


viewCardList : Float -> { sort : Bool } -> List CellType -> Element msg
viewCardList scale { sort } =
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
    , Element.height <| Element.px <| floor <| 200 * scale
    , Element.width <| Element.fill
    ]


viewOne : Float -> Maybe CellType -> Element msg
viewOne scale maybeCellType =
    Element.el
        [ Element.height <| Element.px <| floor <| 200 * scale
        , Element.centerX
        ]
    <|
        case maybeCellType of
            Just cellType ->
                viewSelected scale <|
                    viewContent scale cellType

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


view : Float -> { sort : Bool } -> Maybe (Selected -> msg) -> Maybe Selected -> Deck -> Element msg
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
        , (if maybeSelected == Just First then
            viewSelected scale

           else
            viewSelectable scale First maybeSelectedMsg
          )
          <|
            viewContent scale <|
                Deck.first deck
        , case deck |> Deck.second of
            Just cellType ->
                (if maybeSelected == Just Second then
                    viewSelected scale

                 else
                    viewSelectable scale Second maybeSelectedMsg
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
                , viewCardList scale sort (deck |> Deck.played)
                ]
        ]
