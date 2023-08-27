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

        PointSymbol ->
            "."

        VarSymbol ->
            game.var
                |> Maybe.map Expression.toString
                |> Maybe.withDefault "SAVE"

        ErrSymbol ->
            "ERR"


overlay : { gameWon : Bool } -> Html msg
overlay { gameWon } =
    [ "Divide by Zero" |> Layout.text [ Html.Attributes.style "font-size" "4em", Html.Attributes.style "text-align" "center" ]
    , "A game by Lucas Payr" |> Layout.text [ Html.Attributes.style "font-size" "1.5em" ]
    , [ "There are still" |> Layout.text []
      , "0" |> Layout.text [ Html.Attributes.style "font-size" "4em" ]
      , "secrets to discover." |> Layout.text []
      ]
        |> Layout.column [ Html.Attributes.class "column" ]
    ]
        |> Layout.column
            (Html.Attributes.id "overlay"
                :: (if gameWon then
                        [ Html.Attributes.class "game-won" ]

                    else
                        []
                   )
                ++ Layout.centered
            )


stylesheet : Html msg
stylesheet =
    Html.node "link"
        [ Html.Attributes.rel "stylesheet"
        , Html.Attributes.href "style.css"
        ]
        []
