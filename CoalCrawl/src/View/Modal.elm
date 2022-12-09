module View.Modal exposing (..)

import AnyBag
import Config
import Data.Game exposing (Game)
import Data.Item exposing (Item)
import Data.Modal exposing (Modal)
import Html exposing (Html)
import Html.Attributes as Attr
import Layout
import View.Animation
import View.Button
import View.Title


toHtml : (Maybe ( Int, Item ) -> msg) -> Game -> Modal -> Int -> Html msg
toHtml closeModal game modal level =
    (if level == 1 then
        [ [ View.Title.coal, View.Title.crawl ]
            |> List.map
                (\string ->
                    Html.pre
                        [ Attr.style "font-family" "monospace"
                        , Attr.style "font-size" "8px"
                        ]
                        [ Html.text string ]
                )
            |> Layout.row [ Layout.centerContent ]
        , View.Animation.animate modal.animation modal.animationFrame
            |> Layout.el Layout.centered
        , "Reach the bottom of the cave. Collect coal to progress."
            |> Html.text
            |> Layout.paragraph []
        , "Continue"
            |> View.Button.toHtml (Just (closeModal Nothing))
        ]

     else if level < Config.maxLevel then
        [ "Level Completed"
            |> Html.text
            |> Layout.heading2 [ Layout.contentCentered ]
        , "Items collected sofar: "
            ++ (game
                    |> Data.Game.getTrain
                    |> .items
                    |> AnyBag.toAssociationList
                    |> List.map (\( k, n ) -> String.fromInt n ++ "x " ++ k)
                    |> String.join ", "
               )
            |> Html.text
            |> Layout.el []
        , "Hint: Build a minecart"
            |> Html.text
            |> Layout.heading2 []
        , View.Animation.animate modal.animation modal.animationFrame
            |> Layout.el Layout.centered
        , "Can store up to "
            ++ String.fromInt Config.wagonMaxItems
            ++ " items. You can also push it along."
            |> Html.text
            |> Layout.el []
        , "Choose between 2 Gold or 20 Iron"
            |> Html.text
            |> Layout.el []
        , [ "Get 2 Gold"
                |> View.Button.toHtml
                    (( 2, Data.Item.Gold )
                        |> Just
                        |> closeModal
                        |> Just
                    )
          , "Get 20 Iron"
                |> View.Button.toHtml
                    (( 20, Data.Item.Iron )
                        |> Just
                        |> closeModal
                        |> Just
                    )
          ]
            |> Layout.row []
        ]

     else
        [ "GAME WON!"
            |> Html.text
            |> Layout.heading2 [ Layout.contentCentered ]
        , "Items collected sofar: "
            ++ (game
                    |> Data.Game.getTrain
                    |> .items
                    |> AnyBag.toAssociationList
                    |> List.map (\( k, n ) -> String.fromInt n ++ "x " ++ k)
                    |> String.join ", "
               )
            |> Html.text
            |> Layout.el []
        , "You reached the end of the cave"
            |> Html.text
            |> Layout.heading2 []
        , "Continue"
            |> View.Button.toHtml (Just (closeModal Nothing))
        ]
    )
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
