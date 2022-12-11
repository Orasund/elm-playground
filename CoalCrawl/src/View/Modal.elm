module View.Modal exposing (..)

import AnyBag
import Config
import Data.Animation
import Data.Game exposing (Game)
import Data.Improvement exposing (Improvement)
import Data.Modal exposing (Modal(..))
import Html exposing (Html)
import Html.Attributes as Attr
import Layout
import View.Animation
import View.Button
import View.Title


toHtml : (Maybe Improvement -> msg) -> Game -> Modal -> Int -> Html msg
toHtml closeModal game m level =
    (case m of
        TitleModal modal ->
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
            , View.Animation.animate Data.Animation.tutorial modal.animationFrame
                |> Layout.el Layout.centered
            , "Reach the bottom of the cave. Collect coal to progress."
                |> Html.text
                |> Layout.paragraph []
            , "Continue"
                |> View.Button.toHtml (Just (closeModal Nothing))
            ]

        LevelUpModal improvements ->
            if level < Config.maxLevel then
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
                ]
                    ++ (if List.isEmpty improvements then
                            "Continue"
                                |> View.Button.toHtml (Just (closeModal Nothing))
                                |> List.singleton

                        else
                            [ "Choose an improvement"
                                |> Html.text
                                |> Layout.heading2 []
                            , improvements
                                |> List.map
                                    (\improvement ->
                                        improvement
                                            |> Data.Improvement.toString
                                            |> View.Button.toHtml
                                                (improvement
                                                    |> Just
                                                    |> closeModal
                                                    |> Just
                                                )
                                    )
                                |> Layout.row [ Layout.spacing 8 ]
                            ]
                       )

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
            , Attr.style "width" "300px"
            , Attr.style "background-color" "white"
            , Attr.style "border-radius" "16px"
            , Attr.style "border" "solid 1px black"
            ]
