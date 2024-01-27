module View.Goal exposing (..)

import Game.Card
import Game.Entity
import Goal exposing (Category(..), Goal)
import Html exposing (Attribute, Html)
import Html.Attributes
import Html.Style as Style
import Layout
import Suit exposing (Suit)


cardHeight : { big : Bool } -> Float
cardHeight args =
    if args.big then
        60

    else
        24


cardWidth : { big : Bool } -> Float
cardWidth args =
    0.66 * cardHeight args


fontSize : { big : Bool } -> String
fontSize args =
    if args.big then
        "30px"

    else
        "12px"


viewSuit : List (Attribute msg) -> { big : Bool } -> Maybe Suit -> Html msg
viewSuit attrs args maybeSuit =
    maybeSuit
        |> Maybe.map Suit.icon
        |> Maybe.withDefault "?"
        |> Html.text
        |> Game.Card.back
            ([ Style.height (String.fromFloat (cardHeight args) ++ "px")
             , Html.Attributes.style "font-size" (fontSize args)
             , maybeSuit
                |> Maybe.map Suit.color
                |> Maybe.withDefault "#c6b8b8"
                |> Html.Attributes.style "background-color"
             , Style.boxSizingBorderBox
             , Html.Attributes.style "border" "1px solid rgba(0, 0, 0, 0.2)"
             ]
                ++ attrs
            )


viewSuits : { big : Bool } -> Int -> Maybe Suit -> Html msg
viewSuits args amount maybeSuit =
    let
        offset =
            cardWidth args / 3
    in
    (\attrs ->
        viewSuit attrs args maybeSuit
    )
        |> Game.Entity.new
        |> List.repeat amount
        |> List.indexedMap
            (\i entity ->
                entity
                    |> Game.Entity.move ( toFloat (amount - i - 1) * offset, 0 )
            )
        |> Game.Entity.pileAbove Layout.none
        |> Game.Entity.toHtml
            [ Style.width (String.fromFloat (cardWidth args + (toFloat (amount - 1) * offset)) ++ "px")
            , Style.height (String.fromFloat (cardHeight args) ++ "px")
            ]


viewChallenge : { big : Bool } -> Category -> Html msg
viewChallenge args category =
    case category of
        SingleOf suit ->
            viewSuits args 1 (Just suit)

        PairOf suit ->
            viewSuits args 2 (Just suit)

        Pair ->
            viewSuits args 2 Nothing

        ThreeOf suit ->
            viewSuits args 3 (Just suit)

        ThreeOfAKind ->
            viewSuits args 3 Nothing

        FourOfAKind ->
            viewSuits args 4 Nothing


toHtml : List (Attribute msg) -> { big : Bool } -> Goal -> Html msg
toHtml attrs args list =
    list
        |> List.map (viewChallenge args)
        |> Layout.row
            ([ Style.gap "4px"
             , Style.justifyContentCenter
             ]
                ++ attrs
            )
