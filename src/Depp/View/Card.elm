module Depp.View.Card exposing (toString, view, withActions)

import Cards exposing (Face(..), Suit(..))
import Depp.Data.Deck as Deck exposing (Card)
import Depp.Data.Game as Game exposing (Action, Game)
import Depp.Layout as Layout
import Depp.View as View
import Html exposing (Html)
import Html.Attributes as Attr
import List.Extra as List
import UndoList.Decode exposing (msg)


toString : Game -> Card -> String
toString game card =
    Game.suitToString card.suit game ++ (card |> Game.value game |> String.fromInt)


view : { active : Bool, onClick : Maybe msg } -> Game -> Card -> Html msg
view args game card =
    let
        commonAttr =
            [ Attr.style "text-align" "center"
            , Attr.style "border-radius" "4px"
            , Attr.style "padding" "8px"
            , Attr.style "width" "60px"
            , Attr.style "aspect-ratio" "0.66"
            ]

        label =
            card |> toString game
    in
    case args.onClick of
        Just onClick ->
            View.selectButton
                commonAttr
                ( args.active
                , { label = label
                  , onClick = Just onClick
                  }
                )

        Nothing ->
            Html.div
                ([ Attr.style "border" "1px solid rgba(0, 0, 0, 0.1)"
                 ]
                    ++ commonAttr
                )
                [ Html.text label ]


withActions : List { label : String, onClick : Maybe msg } -> Game -> Card -> Html msg
withActions list game card =
    list
        |> View.actionGroup (toString game card)
        |> List.singleton
        |> Html.p []
        |> List.singleton
        |> Html.aside []
        |> List.singleton
        |> Html.article []
