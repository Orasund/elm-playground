module LittleWorldPuzzler.State.Playing exposing (Model, Msg, init, update, view)

import Browser
import Browser.Dom as Dom
import Browser.Events exposing (onResize)
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Framework.Modifier as Modifier exposing (Modifier(..))
import Grid.Bordered as Grid exposing (Grid)
import Grid.Position as Position exposing (Position)
import Html exposing (Html)
import LittleWorldPuzzler.Automata as Automata
import LittleWorldPuzzler.Data.Board as Board exposing (Board)
import LittleWorldPuzzler.Data.CellType as CellType exposing (CellType(..))
import LittleWorldPuzzler.Data.Deck as Deck exposing (Deck, Selected(..))
import LittleWorldPuzzler.View.Board as BoardView
import LittleWorldPuzzler.View.Button as Button
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



----------------------
-- Model
----------------------


type alias State =
    { board : Board
    , deck : Deck
    , selected : Maybe Selected
    , score : Int
    }


type alias Model =
    ( State, Seed )


type Msg
    = Selected Selected
    | PositionSelected Position
    | CardPlaced



----------------------
-- Init
----------------------


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


init : Seed -> Model
init seed =
    Random.step stateGenerator seed



----------------------
-- Update
----------------------


play : ( State, Seed ) -> ( Model, Cmd Msg )
play ( state, seed ) =
    let
        seconds : Float
        seconds =
            1000
    in
    ( ( state, seed )
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ( { board, deck, score } as state, seed ) =
    let
        defaultCase : ( Model, Cmd Msg )
        defaultCase =
            ( ( state, seed ), Cmd.none )
    in
    case msg of
        Selected select ->
            ( ( { state | selected = Just select }, seed ), Cmd.none )

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
            ( ( { state
                    | board =
                        board
                            |> Grid.map (Automata.step (board |> Grid.toDict))
                    , score = score + 1
                }
              , seed
              )
            , Cmd.none
            )



----------------------
-- View
----------------------


view : Float -> msg -> (Msg -> msg) -> Model -> Element msg
view scale restartMsg msgMapper ( { board, deck, selected, score }, _ ) =
    Element.column
        [ Element.centerY
        , Element.centerX
        , Element.spacing 5
        ]
        [ Element.row
            [ Element.spaceEvenly
            , Element.centerX
            , Element.width <| Element.px <| floor <| 608 * scale
            ]
          <|
            [ Element.el
                [ Element.width <| Element.px <| floor <| 150 * scale
                ]
              <|
                Element.text ""
            , Element.el [ Font.size <| floor <| 50 * scale ] <|
                Element.text <|
                    String.fromInt score
            , Button.view
                [ Element.width <| Element.px <| floor <| 150 * scale
                , Element.padding <| floor <| 7 * scale
                , Border.rounded (floor <| 10 * scale)
                , Font.size <| floor <| 36 * scale
                , Font.family
                    [ Font.sansSerif ]
                ]
              <|
                { onPress = Just restartMsg
                , label = Element.text "Restart"
                }

            --Button.buttonWidth [ Medium ] (Just Restart) "Restart" (floor <| 120 * scale)
            ]
        , Element.column
            ([ Element.spacing (floor <| 5 * scale)
             , Background.color <| Element.rgb255 242 242 242
             , Element.padding (floor <| 20 * scale)
             , Border.rounded (floor <| 10 * scale)
             ]
                |> (if board |> Grid.emptyPositions |> (==) [] then
                        (::) (Element.alpha 0.3)

                    else
                        identity
                   )
            )
            [ BoardView.view scale (msgMapper << PositionSelected) board
            , DeckView.view scale (msgMapper << Selected) selected deck
            ]
        ]
