module Game exposing (..)

import Expression exposing (Expression(..), Operator(..), Symbol(..))


type alias Game =
    { expression : Expression
    , level : Int
    , var : Maybe Expression
    }


new : Game
new =
    { expression = Expression.Number 0
    , level = 1
    , var = Nothing
    }


loadLevel : Int -> Game -> Game
loadLevel level game =
    { game
        | expression = Expression.Number 0
        , level = level
    }


deleteInput : Game -> Game
deleteInput game =
    { game
        | expression = game.expression |> Expression.delete
    }


applyOperator : Int -> Int -> Operator -> Expression
applyOperator i1 i2 operator =
    case operator of
        TimesOp ->
            i2 * i1 |> Number

        DividedOp ->
            if i1 == 0 then
                DivisionByZero

            else
                i2 // i1 |> Number

        PowOp ->
            i2 ^ i1 |> Number

        RootOp ->
            toFloat i2
                ^ (1 / toFloat i1)
                |> round
                |> Number


addSymbol : Symbol -> Game -> Game
addSymbol symbol game =
    case symbol of
        NumberSymbol i1 ->
            (case game.expression of
                Number i2 ->
                    String.fromInt i2
                        ++ String.fromInt i1
                        |> String.toInt
                        |> Maybe.map Number
                        |> Maybe.withDefault Error

                Op operator exp2 ->
                    case exp2 of
                        Number i2 ->
                            applyOperator i1 i2 operator

                        Op _ _ ->
                            Error

                        Error ->
                            Error

                        DivisionByZero ->
                            DivisionByZero

                Error ->
                    Error

                DivisionByZero ->
                    DivisionByZero
            )
                |> setExpressionTo game

        OpSymbol operator ->
            (case ( operator, game.expression ) of
                ( TimesOp, Op TimesOp exp2 ) ->
                    Op PowOp exp2

                ( DividedOp, Op DividedOp exp2 ) ->
                    Op RootOp exp2

                _ ->
                    Op operator game.expression
            )
                |> setExpressionTo game

        VarSymbol ->
            case game.var of
                Nothing ->
                    { game
                        | expression = Number 0
                        , var = Just game.expression
                    }

                Just exp ->
                    exp
                        |> Expression.toSymbols
                        |> List.foldl addSymbol
                            { game | var = Nothing }

        ErrSymbol ->
            Error |> setExpressionTo game


setExpressionTo : Game -> Expression -> Game
setExpressionTo game expression =
    { game | expression = expression }
