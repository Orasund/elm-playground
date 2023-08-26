module View exposing (..)

import Expression exposing (Operator(..), Symbol(..))
import Game exposing (Game)
import Html exposing (Html)
import Html.Attributes
import Layout


viewInput : Game -> Symbol -> String
viewInput game input =
    case input of
        NumberSymbol n ->
            String.fromInt n

        OpSymbol op ->
            Expression.opToString op

        VarSymbol ->
            game.var
                |> Maybe.map Expression.toString
                |> Maybe.withDefault "VAR"

        ErrSymbol ->
            "ERR"


stylesheet : Html msg
stylesheet =
    Html.node "link"
        [ Html.Attributes.rel "stylesheet"
        , Html.Attributes.href "style.css"
        ]
        []
