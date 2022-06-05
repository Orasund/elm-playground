module Main exposing (main)

import Browser exposing (Document)
import Game exposing (Game)
import Html.Styled
import Random exposing (Seed)
import Time
import View.Grid as Grid
import View.Natural as Natural


type alias Model =
    { game : Game
    , phase : Maybe Phase
    , seed : Seed
    }


type Phase
    = Falling { final : Bool }
    | Matching { final : Bool }
    | Moving


type Msg
    = Tick
    | TileClicked Int


init : () -> ( Model, Cmd Msg )
init () =
    ( { game = Game.new
      , phase = Nothing
      , seed = Random.initialSeed 42
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 100 (\_ -> Tick)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick ->
            case model.phase of
                Just (Falling args) ->
                    ( model.game
                        |> Game.fall
                        |> Maybe.map (\game -> { model | game = game })
                        |> Maybe.withDefault { model | phase = Just (Matching args) }
                    , Cmd.none
                    )

                Just (Matching args) ->
                    ( model.game
                        |> Game.match
                        |> Maybe.map
                            (\game ->
                                { model
                                    | game = game
                                    , phase = Just (Falling args)
                                }
                            )
                        |> Maybe.withDefault
                            { model
                                | phase =
                                    if args.final then
                                        Nothing

                                    else
                                        Just Moving
                            }
                    , Cmd.none
                    )

                Just Moving ->
                    ( model.game
                        |> Game.move
                        |> (\generator -> Random.step generator model.seed)
                        |> (\( game, seed ) ->
                                { model
                                    | game = game
                                    , phase = Just (Falling { final = True })
                                }
                           )
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        TileClicked x ->
            ( model.game
                |> Game.place x
                |> Maybe.map
                    (\generator ->
                        model.seed
                            |> Random.step generator
                            |> (\( game, seed ) ->
                                    { model
                                        | game = game
                                        , phase = Just (Falling { final = False })
                                        , seed = seed
                                    }
                               )
                    )
                |> Maybe.withDefault model
            , Cmd.none
            )


view : Model -> Document Msg
view model =
    { title = "Bugzzle"
    , body =
        [ model.game.grid
            |> Grid.view TileClicked
        , [ Natural.view model.game.next |> Html.Styled.text
          ]
            |> Html.Styled.p []
        , (case model.phase of
            Nothing ->
                ""

            Just (Falling _) ->
                "Falling"

            Just (Matching _) ->
                "Matching"

            Just Moving ->
                "Moving"
          )
            |> Html.Styled.text
        ]
            |> List.map Html.Styled.toUnstyled
    }


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
