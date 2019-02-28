module LittleWorldPuzzler.Automata.Neighborhood exposing (fromList, fullSymmetry, toList, toString)

import CellAutomata exposing (Neighborhood, RuleExpression(..), Symmetry, anyNeighborhood)
import Dict
import LittleWorldPuzzler.Data.CellType as CellType exposing (CellType)


setNorth :
    ( List ( Int, Maybe state ), Neighborhood (RuleExpression (Maybe state)) )
    -> ( List ( Int, Maybe state ), Neighborhood (RuleExpression (Maybe state)) )
setNorth ( list, neighborHood ) =
    case list of
        ( n, maybeState ) :: tail ->
            if n == 1 then
                ( tail, { neighborHood | north = Exactly maybeState } )

            else if n > 1 then
                ( ( n - 1, maybeState ) :: tail, { neighborHood | north = Exactly maybeState } )

            else
                ( list, neighborHood )

        [] ->
            ( list, neighborHood )


setEast :
    ( List ( Int, Maybe state ), Neighborhood (RuleExpression (Maybe state)) )
    -> ( List ( Int, Maybe state ), Neighborhood (RuleExpression (Maybe state)) )
setEast ( list, neighborHood ) =
    case list of
        ( n, maybeState ) :: tail ->
            if n == 1 then
                ( tail, { neighborHood | east = Exactly maybeState } )

            else if n > 1 then
                ( ( n - 1, maybeState ) :: tail, { neighborHood | east = Exactly maybeState } )

            else
                ( list, neighborHood )

        [] ->
            ( list, neighborHood )


setSouth :
    ( List ( Int, Maybe state ), Neighborhood (RuleExpression (Maybe state)) )
    -> ( List ( Int, Maybe state ), Neighborhood (RuleExpression (Maybe state)) )
setSouth ( list, neighborHood ) =
    case list of
        ( n, maybeState ) :: tail ->
            if n == 1 then
                ( tail, { neighborHood | south = Exactly maybeState } )

            else if n > 1 then
                ( ( n - 1, maybeState ) :: tail, { neighborHood | south = Exactly maybeState } )

            else
                ( list, neighborHood )

        [] ->
            ( list, neighborHood )


setWest :
    ( List ( Int, Maybe state ), Neighborhood (RuleExpression (Maybe state)) )
    -> ( List ( Int, Maybe state ), Neighborhood (RuleExpression (Maybe state)) )
setWest ( list, neighborHood ) =
    case list of
        ( n, maybeState ) :: tail ->
            if n == 1 then
                ( tail, { neighborHood | west = Exactly maybeState } )

            else if n > 1 then
                ( ( n - 1, maybeState ) :: tail, { neighborHood | west = Exactly maybeState } )

            else
                ( list, neighborHood )

        [] ->
            ( list, neighborHood )


fromList : List ( Int, Maybe state ) -> Neighborhood (RuleExpression (Maybe state))
fromList list =
    ( list, anyNeighborhood )
        |> setNorth
        |> setEast
        |> setSouth
        |> setWest
        |> Tuple.second


toList : Neighborhood (RuleExpression (Maybe state)) -> List ( Int, Maybe state )
toList neighbors =
    [ .north, .east, .south, .west ]
        |> List.foldr
            (\get list ->
                case neighbors |> get of
                    Exactly elem ->
                        case list of
                            ( n, maybeState ) :: tail ->
                                if maybeState == elem then
                                    ( n + 1, maybeState ) :: tail

                                else
                                    ( 1, elem ) :: list

                            [] ->
                                [ ( 1, elem ) ]

                    Anything ->
                        list
            )
            []


fullSymmetry : Symmetry CellType
fullSymmetry maybeCellType { north, east, south, west } { from, to, neighbors } =
    let
        dict =
            [ north, east, south, west ]
                |> List.map (Maybe.map CellType.toInt >> Maybe.withDefault 0)
                |> List.sort
                |> List.foldr
                    (\elem list ->
                        case list of
                            ( maybeState, n ) :: tail ->
                                if maybeState == elem then
                                    ( maybeState, n + 1 ) :: tail

                                else
                                    ( elem, 1 ) :: list

                            [] ->
                                [ ( elem, 1 ) ]
                    )
                    []
                |> Dict.fromList
    in
    if
        maybeCellType
            == from
            && (neighbors
                    |> toList
                    |> List.filter
                        (\( minN, maybeElem ) ->
                            dict
                                |> Dict.get (maybeElem |> Maybe.map CellType.toInt |> Maybe.withDefault 0)
                                |> Maybe.andThen
                                    (\n ->
                                        if n >= minN then
                                            Just ()

                                        else
                                            Nothing
                                    )
                                |> (==) Nothing
                        )
                    |> (==) []
               )
    then
        Just to

    else
        Nothing


toString : Neighborhood (RuleExpression (Maybe CellType)) -> String
toString { north, east, south, west } =
    let
        expressionToString : RuleExpression (Maybe CellType) -> String
        expressionToString direction =
            case direction of
                Exactly maybeCellType ->
                    maybeCellType
                        |> Maybe.map CellType.toString
                        |> Maybe.withDefault " "

                --"⭕"
                Anything ->
                    "▪"

        --"❓"
    in
    (north |> expressionToString)
        ++ (east |> expressionToString)
        ++ (south |> expressionToString)
        ++ (west |> expressionToString)
