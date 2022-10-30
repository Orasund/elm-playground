module FactoryCity.View.Board exposing (view)

import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import FactoryCity.Data.CellType as CellType exposing (CellType)
import FactoryCity.Data.Item as Item
import FactoryCity.View.Text as Text
import Framework.Grid as Grid
import Grid.Bordered as Grid exposing (Grid)


viewCell : Float -> ( Int, Int ) -> Maybe (( Int, Int ) -> msg) -> Maybe CellType -> Element msg
viewCell scale position maybeMsg maybeCellType =
    Element.el
        ([ Element.centerX
         , Border.width 1
         , Border.color <| Element.rgba255 219 219 219 1
         , Element.width <| Element.px <| round <| scale * 100
         , Element.height <| Element.px <| round <| scale * 100
         ]
            ++ (case maybeMsg of
                    Just msg ->
                        [ Events.onClick <| msg position ]

                    Nothing ->
                        []
               )
            ++ (case maybeCellType of
                    Nothing ->
                        []

                    Just cellType ->
                        cellType
                            |> .item
                            |> Maybe.map
                                (Item.color
                                    >> (\( r, g, b ) ->
                                            [ Background.color <| Element.rgb255 r g b ]
                                       )
                                )
                            |> Maybe.withDefault []
               )
        )
    <|
        Element.column
            [ Element.centerY
            , Element.centerX
            , Font.center
            , Element.spacing <| floor <| scale * 10
            ]
        <|
            case maybeCellType of
                Just cellType ->
                    cellType
                        |> (CellType.toString
                                >> (\( sort, item ) ->
                                        [ Text.colored 16 <| sort
                                        , Element.text <| item
                                        ]
                                   )
                           )
                        |> List.map
                            (List.singleton
                                >> Element.paragraph
                                    [ Element.centerY
                                    , Element.centerX
                                    , Font.center
                                    ]
                            )

                Nothing ->
                    []


view : { scale : Float, maybePositionMsg : Maybe (( Int, Int ) -> msg) } -> Grid CellType -> Element msg
view { scale, maybePositionMsg } grid =
    Element.column
        (Grid.compact
            ++ [ Element.centerX
               , Element.width <| Element.shrink
               , Element.height <| Element.px <| floor <| scale * 400
               ]
        )
    <|
        (grid
            |> Grid.foldr
                (\( x, y ) maybeCellType ( workingRow, list ) ->
                    let
                        newRow : List (Element msg)
                        newRow =
                            viewCell scale ( x, y ) maybePositionMsg maybeCellType
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
