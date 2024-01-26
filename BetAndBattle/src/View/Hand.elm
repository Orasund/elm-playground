module View.Hand exposing (..)

import Game exposing (Card)
import Goal
import Html exposing (Html)
import Html.Style as Style
import Layout
import View.Card


toHtml : { onPlay : Card -> msg, currentPercentage : Int } -> List Card -> Html msg
toHtml args list =
    list
        |> List.map
            (\card ->
                card
                    |> View.Card.toHtml
                        (Layout.asButton
                            { label = "play"
                            , onPress =
                                if Goal.probability card.goal <= args.currentPercentage then
                                    Just (args.onPlay card)

                                else
                                    Nothing
                            }
                        )
            )
        |> Layout.row [ Style.gap "8px" ]


opponent : List Card -> Html msg
opponent list =
    list
        |> List.map (\_ -> View.Card.back)
        |> Layout.row [ Style.gap "8px" ]
