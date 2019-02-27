module Main exposing (main)

import Browser
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Framework.Button as Button
import Framework.Modifier as Modifier exposing (Modifier(..))
import Grid.Bordered as Grid exposing (Grid)
import Grid.Position as Position exposing (Position)
import Html exposing (Html)
import LittleWorldPuzzler.Automata as Automata
import LittleWorldPuzzler.Data.Board as Board exposing (Board)
import LittleWorldPuzzler.Data.CellType as CellType exposing (CellType(..))
import LittleWorldPuzzler.Data.Deck as Deck exposing (Deck, Selected(..))
import LittleWorldPuzzler.View.Board as BoardView
import LittleWorldPuzzler.View.Deck as DeckView
import Process
import Random exposing (Generator, Seed)
import Task


columns : Int
columns =
    4


rows : Int
rows =
    columns


type alias State =
    { board : Board
    , deck : Deck
    , selected : Maybe Selected
    , score : Int
    }


type alias Model =
    Maybe ( State, Seed )


type Msg
    = GotSeed Seed
    | Selected Selected
    | PositionSelected Position
    | CardPlaced
    | Restart


stateGenerator : Generator State
stateGenerator =
    [ Wood
    , Wood
    , Wood
    , Wood
    , Water
    , Water
    , Water
    , Water
    , Stone
    , Fire
    ]
        |> Deck.fromList
        |> Deck.shuffle
        |> Random.map
            (\deck ->
                { board =
                    Grid.empty { columns = columns, rows = rows }
                , deck = deck
                , selected = Nothing
                , score = 0
                }
            )


init : () -> ( Model, Cmd Msg )
init _ =
    ( Nothing
    , Random.generate GotSeed Random.independentSeed
    )


play : ( State, Seed ) -> ( Model, Cmd Msg )
play ( state, seed ) =
    let
        seconds : Float
        seconds =
            1000
    in
    ( Just ( state, seed )
    , Task.perform (always CardPlaced) <| Process.sleep (0.1 * seconds)
    )


playFirst : Position -> ( State, Seed ) -> ( Model, Cmd Msg )
playFirst position ( { board } as state, seed ) =
    Random.step
        (Deck.playFirst state.deck
            |> Random.map
                (\deck ->
                    { state
                        | deck = deck
                        , selected = Nothing
                        , board =
                            board
                                |> Board.place position
                                    (state.deck |> Deck.first)
                    }
                )
        )
        seed
        |> play


playSecond : Position -> CellType -> State -> (Seed -> ( Model, Cmd Msg ))
playSecond position cellType ({ board, deck } as state) =
    { state
        | deck = deck |> Deck.playSecond
        , selected = Nothing
        , board = board |> Board.place position cellType
    }
        |> (\a b ->
                play ( a, b )
           )


updateState : Msg -> ( State, Seed ) -> ( Model, Cmd Msg )
updateState msg ( { board, deck } as state, seed ) =
    let
        defaultCase : ( Model, Cmd Msg )
        defaultCase =
            ( Just ( state, seed ), Cmd.none )
    in
    case msg of
        Selected select ->
            ( Just ( { state | selected = Just select }, seed ), Cmd.none )

        PositionSelected position ->
            case state.selected of
                Just First ->
                    playFirst position ( state, seed )

                Just Second ->
                    case deck |> Deck.second of
                        Just second ->
                            playSecond position second state seed

                        Nothing ->
                            playFirst position ( state, seed )

                Nothing ->
                    defaultCase

        CardPlaced ->
            ( Just
                ( { state
                    | board =
                        board
                            |> Grid.map (Automata.step (board |> Grid.toDict))
                  }
                , seed
                )
            , Cmd.none
            )

        Restart ->
            init ()

        GotSeed _ ->
            defaultCase


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        defaultCase : ( Model, Cmd Msg )
        defaultCase =
            ( model, Cmd.none )
    in
    case model of
        Just stateWithSeed ->
            updateState msg stateWithSeed

        Nothing ->
            case msg of
                GotSeed seed ->
                    ( Just (Random.step stateGenerator seed), Cmd.none )

                _ ->
                    defaultCase


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Html Msg
view model =
    Element.layout
        [ Font.family
            [ Font.external
                { url = "font.css"
                , name = "Noto Emoji"
                }
            ]
        , Background.color <| Element.rgb255 44 48 51
        ]
    <|
        case model of
            Just ( { board, deck, selected, score }, _ ) ->
                Element.column
                    [ Element.centerY
                    , Element.centerX
                    , Element.spacing 5
                    ]
                    [ Element.row
                        [ Element.spaceEvenly
                        , Element.centerX
                        , Element.width <| Element.px <| 608
                        ]
                      <|
                        [ Element.el [ Element.width <| Element.px <| 120 ] <|
                            Element.text ""
                        , Element.el [ Font.size 50 ] <|
                            Element.text <|
                                String.fromInt score
                        , Element.el
                            [ Element.width <| Element.px <| 120 ]
                          <|
                            Button.buttonWidth [ Medium ] (Just Restart) "Restart" 120
                        ]
                    , Element.column
                        ([ Element.spacing 10
                         , Background.color <| Element.rgb255 242 242 242
                         , Element.padding 20
                         , Border.rounded 10
                         ]
                            |> (if board |> Grid.emptyPositions |> (==) [] then
                                    (::) (Element.alpha 0.3)

                                else
                                    identity
                               )
                        )
                        [ BoardView.view PositionSelected board
                        , DeckView.view Selected selected deck
                        ]
                    ]

            Nothing ->
                Element.text ""


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
