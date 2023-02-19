module Chapter.Card exposing (..)

import ElmBook.Actions
import ElmBook.Chapter exposing (Chapter)
import Example.FlippableCard
import Html
import View.Card
import View.Component


type alias Model =
    { flippableCard : Example.FlippableCard.Model }


init : Model
init =
    { flippableCard = Example.FlippableCard.init }


chapter : { get : model -> Model, setTo : model -> Model -> model } -> Chapter model
chapter args =
    ElmBook.Chapter.chapter "Game.Card"
        |> ElmBook.Chapter.withChapterInit
            (\model ->
                ( args.setTo model init, Cmd.none )
            )
        |> ElmBook.Chapter.withStatefulComponentList
            [ ( "Styles"
              , \_ ->
                    View.Component.list
                        [ ( "empty", View.Card.empty )
                        , ( "default", View.Card.default )
                        , ( "back", View.Card.back )
                        ]
              )
            , ( "Ratios"
              , \_ ->
                    View.Component.list
                        [ ( "ratio (2/3)", View.Card.default )
                        , ( "ratio 1", View.Card.square )
                        , ( "ratio (3/2)", View.Card.horizontal )
                        ]
              )
            , ( "Layouts"
              , \_ ->
                    View.Component.list
                        [ ( "header", View.Card.titleRow )
                        , ( "fillingImage", View.Card.fullImage )
                        , ( "description", View.Card.imageAndDesc )
                        ]
              )
            , ( "Transformations"
              , \_ ->
                    View.Component.list
                        [ ( "rotate (pi/2)", View.Card.rotated )
                        , ( "scale (1/2)", View.Card.small )
                        , ( "translate (0,-50)", View.Card.drawn )
                        , ( "flip (pi/4)", View.Card.flipped )
                        ]
              )
            , ( "Flippable Cards"
              , \model ->
                    model
                        |> args.get
                        |> (\state ->
                                View.Component.list
                                    [ ( "Click to flip the card"
                                      , Example.FlippableCard.view state.flippableCard
                                            |> Html.map
                                                (\msg ->
                                                    ElmBook.Actions.updateState
                                                        (\m ->
                                                            m
                                                                |> args.get
                                                                |> (\s ->
                                                                        s.flippableCard
                                                                            |> Example.FlippableCard.update msg
                                                                            |> (\a -> { s | flippableCard = a })
                                                                   )
                                                                |> args.setTo m
                                                        )
                                                )
                                      )
                                    ]
                           )
              )
            ]
        |> ElmBook.Chapter.renderWithComponentList
            ""
