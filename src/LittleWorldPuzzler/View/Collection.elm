module LittleWorldPuzzler.View.Collection exposing (view)

import Element exposing (Attribute, Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import LittleWorldPuzzler.Data.CellType as CellType exposing (CellType(..))
import LittleWorldPuzzler.View.Button as Button
import LittleWorldPuzzler.View.Deck as DeckView
import Set exposing (Set)


attributes : Float -> List (Attribute msg)
attributes scale =
    [ Element.spacingXY 0 (floor <| 10 * scale)
    , Element.centerX
    , Element.width <| Element.fill
    , Element.alignTop
    ]


viewCell : Float -> (CellType -> msg) -> Set String -> CellType -> Element msg
viewCell scale msgMapper set cellType =
    Element.column
        [ Element.width <| Element.fill
        , Element.alignTop
        , Element.spacing <| floor <| scale * 10
        ]
    <|
        [ Element.el [ Element.centerX, Font.center, Font.size (floor <| 20 * scale) ] <|
            Element.text "⬇"
        , Button.view
            ([ Element.centerX
             , Font.center
             , Element.height <| Element.px <| floor <| scale * 50
             , Element.width <| Element.px <| floor <| scale * 50
             ]
                |> (if set |> Set.member (cellType |> CellType.toString) then
                        identity

                    else
                        List.append [ Background.color <| Element.rgb255 242 242 242 ]
                   )
            )
          <|
            if set |> Set.member (cellType |> CellType.toString) then
                { onPress = Just <| msgMapper cellType
                , label = Element.text <| CellType.toString <| cellType
                }

            else
                { onPress = Just <| msgMapper cellType
                , label =
                    Element.text <| "❓"
                }
        ]


tree : Float -> (CellType -> msg) -> Set String -> Element msg
tree scale msgMapper set =
    Element.el
        [ Element.spaceEvenly
        , Background.color <| Element.rgb255 242 242 242
        , Element.padding (floor <| 90 * scale)
        , Border.rounded (floor <| 10 * scale)
        , Element.width <| Element.fill
        , Element.height <| Element.px <| floor <| scale * 568
        , Font.size (floor <| 40 * scale)
        ]
    <|
        Element.row (attributes scale) <|
            [ Element.column (attributes scale) <|
                ([ Stone, Glacier, Ice, Snow ]
                    |> List.map (viewCell scale msgMapper set)
                )
            , Water |> viewCell scale msgMapper set
            , Element.column (attributes scale) <|
                ([ Wood, Evergreen ]
                    |> List.map (viewCell scale msgMapper set)
                )
            , Element.column (attributes scale) <|
                [ Fire |> viewCell scale msgMapper set
                , Element.row (attributes scale) <|
                    [ Desert |> viewCell scale msgMapper set
                    , Element.column (attributes scale) <|
                        ([ Volcano, Fog ] |> List.map (viewCell scale msgMapper set))
                    ]
                ]
            ]


view : Float -> (CellType -> msg) -> Set String -> Maybe CellType -> Element msg
view scale msgMapper set maybeCellType =
    Element.column
        [ Element.spacing (floor <| 5 * scale)
        , Background.color <| Element.rgb255 242 242 242
        , Element.padding (floor <| 20 * scale)
        , Border.rounded (floor <| 10 * scale)
        , Element.width <| Element.fill
        ]
    <|
        [ tree scale msgMapper set
        , DeckView.viewOne scale maybeCellType
        ]
