module LittleWorldPuzzler.View.Board exposing (view)

import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Framework.Grid as Grid
import Grid.Bordered as Grid exposing (Grid)
import Grid.Position exposing (Position)
import LittleWorldPuzzler.Data.CellType as CellType exposing (CellType)
import LittleWorldPuzzler.View.Rule as RuleView


viewCell : Float -> Position -> Maybe (Position -> msg) -> Maybe CellType -> Element msg
viewCell scale position maybeMsg maybeCellType =
    Element.el
        ([ Element.centerX
         , Border.width 1
         , Border.color <| Element.rgba255 219 219 219 1
         , Element.width <| Element.px <| floor <| scale * 100
         , Element.height <| Element.px <| floor <| scale * 100
         , Element.inFront <|
            Element.el
                [ Element.height <| Element.fill
                , Element.width <| Element.fill
                , Background.color <| Element.rgb255 242 242 242
                , Element.mouseOver [ Element.transparent True ]
                ]
            <|
                Element.el
                    [ Element.centerY
                    , Font.size <| floor <| scale * 90
                    , Element.centerX
                    , Font.center
                    ]
                <|
                    Element.text <|
                        (maybeCellType
                            |> Maybe.map CellType.toString
                            |> Maybe.withDefault ""
                        )
         ]
            |> (case maybeMsg of
                        Just msg ->
                            (::) (Events.onClick <| msg position)

                        Nothing ->
                            identity
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
                    [ Element.el
                        [ Font.size <| floor <| scale * 50
                        , Font.center
                        , Element.centerX
                        ]
                      <|
                        Element.text <|
                            (cellType |> CellType.toString)
                    , Element.column
                        [ Font.size <| floor <| scale * 10
                        , Element.spacing <| floor <| scale * 5
                        , Element.centerX
                        ]
                        (RuleView.view cellType)
                    ]

                Nothing ->
                    []


view : Float -> Maybe (Position -> msg) -> Grid CellType -> Element msg
view scale maybePositionMsg grid =
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
