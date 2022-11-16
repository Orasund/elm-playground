module View.Modal exposing (..)

import AnyBag
import Config
import Data.Game exposing (Game)
import Data.Modal exposing (Modal)
import Html exposing (Html)
import Html.Attributes as Attr
import Layout
import View.Animation
import View.Button


toHtml : msg -> Game -> Modal -> Html msg
toHtml closeModal game modal =
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
    , "Hint: Build a Wagon"
        |> Html.text
        |> Layout.heading2 []
    , "Build a Wagon by clicking on an empty floor tile and select \"Build Wagon\""
        |> Html.text
        |> Layout.paragraph []
    , View.Animation.animate modal.animation modal.animationFrame
        |> Layout.el Layout.centered
    , "Can store up to "
        ++ String.fromInt Config.wagonMaxItems
        ++ " items. You can also push it along."
        |> Html.text
        |> Layout.el []
    , "Continue"
        |> View.Button.toHtml (Just closeModal)
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
            , Attr.style "border" "solid 1px black"
            ]
