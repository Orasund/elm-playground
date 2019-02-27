module LittleWorldPuzzler.View.Board exposing (view)

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
import LittleWorldPuzzler.Data.CellType as CellType exposing (CellType)


viewCell : msg -> Maybe CellType -> Element msg
viewCell msg maybeCellType =
    Element.el
        [ Font.size 100
        , Element.centerX
        , Border.width 1
        , Border.color <| Element.rgba255 219 219 219 1
        , Element.width <| Element.px <| 142
        , Element.height <| Element.px <| 142
        , Events.onClick msg
        ]
    <|
        Element.el [ Element.centerY, Element.centerX, Font.center ] <|
            Element.text <|
                (maybeCellType
                    |> Maybe.map CellType.toString
                    |> Maybe.withDefault ""
                )


view : (Position -> msg) -> Grid CellType -> Element msg
view positionMsg grid =
    Element.column [ Element.spaceEvenly, Element.centerX ] <|
        (grid
            |> Grid.foldr
                (\( x, y ) maybeCellType ( workingRow, list ) ->
                    let
                        newRow : List (Element msg)
                        newRow =
                            viewCell (positionMsg ( x, y )) maybeCellType
                                :: workingRow
                    in
                    if y == 0 then
                        ( [], newRow :: list )

                    else
                        ( newRow, list )
                )
                ( [], [] )
            |> Tuple.second
            |> List.map
                (Element.row
                    []
                )
        )
