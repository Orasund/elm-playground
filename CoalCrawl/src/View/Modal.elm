module View.Modal exposing (..)

import AnyBag
import Config
import Data.Game exposing (Game)
import Html exposing (Html)
import Html.Attributes as Attr
import Layout
import View.Button


toHtml : msg -> Game -> Html msg
toHtml closeModal game =
    [ "Level Completed"
        |> Html.text
        |> Layout.heading1 [ Layout.contentCentered ]
    , "Items collected sofar: "
        ++ (game.train.items
                |> AnyBag.toAssociationList
                |> List.map (\( k, n ) -> String.fromInt n ++ "x " ++ k)
                |> String.join ", "
           )
        |> Html.text
        |> Layout.el []
    , "New Crafting Recipe unlocked:"
        |> Html.text
        |> Layout.el []
    , Html.node "style"
        []
        [ """@keyframes newRecipe {
  from {
    transform: rotate(-10deg);
  }
  to {
    transform: rotate(10deg);
  }""" |> Html.text
        ]
    , "W"
        |> Html.text
        |> Layout.el
            [ Attr.style "animation" "newRecipe 2s infinite ease-in-out alternate"
            , Attr.style "font-size" "64px"
            , Attr.style "height" "64px"
            , Attr.style "width" "64px"
            ]
        |> Layout.el [ Layout.centerContent ]
    , "Wagon (Needs "
        ++ String.fromInt Config.wagonCost
        ++ " Iron)"
        |> Html.text
        |> Layout.el [ Layout.alignAtCenter, Layout.centerContent ]
    , "Can store up to "
        ++ String.fromInt Config.wagonMaxItems
        ++ " items. You can also push it along."
        |> Html.text
        |> Layout.el []
    , "Continue"
        |> View.Button.toHtml closeModal
    ]
        |> Layout.column
            [ Attr.style "padding" "16px"
            , Attr.style "width" "100%"
            , Attr.style "height" "100%"
            ]
        |> Layout.el
            [ Attr.style "position" "absolute"
            , Attr.style "top" "10%"
            , Attr.style "left" "10%"
            , Attr.style "width" "80%"
            , Attr.style "height" "80%"
            , Attr.style "background-color" "white"
            , Attr.style "border-radius" "16px"
            ]
