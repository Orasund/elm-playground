module Ecocards.Page.LocalGame exposing (Model, Msg, init, update, view)

import Array
import Bag exposing (Bag)
import Browser
import Dict exposing (Dict)
import Dict.Extra as Dict
import Ecocards.Data.Animal as Animal exposing (Animal, Behaviour(..))
import Ecocards.Data.Bag as Bag
import Ecocards.Data.Game as Game exposing (Game)
import Ecocards.Data.GameArea as GameArea exposing (GameArea)
import Ecocards.Data.GamePhase as GamePhase exposing (GamePhase(..))
import Ecocards.Data.Move as Move exposing (Move)
import Ecocards.View as View
import Ecocards.View.Color as Color
import Element exposing (Element)
import Element.Input as Input
import Form.Decoder exposing (errors)
import Html.Attributes exposing (selected)
import List.Extra as List
import PixelEngine exposing (game)
import Random exposing (Seed)
import Set exposing (Set)
import Set.Extra as Set
import Widget
import Widget.Style exposing (ButtonStyle, ColumnStyle, DialogStyle, ExpansionPanelStyle, LayoutStyle, RowStyle, SortTableStyle, TabStyle, TextInputStyle)
import Widget.Style.Material as Material exposing (Palette)


type alias Model =
    { game : Game
    , phase : GamePhase
    , error : Maybe String
    , useAutoTap : Bool
    , seed : Seed
    }


type Msg
    = ClickedCard { id : Int }
    | PlayedCard { index : Int }
    | Canceled
    | Confirmed
    | ToggleAutoTap


init : Seed -> ( Model, Cmd Msg )
init =
    Random.step
        (Random.map2
            (\yourArea oppArea ->
                { yourArea = yourArea
                , oppArea = oppArea
                , animals = Dict.empty
                , nextId = 1
                }
            )
            ([ Animal.wolf, Animal.fish, Animal.mouse, Animal.cat, Animal.mouse, Animal.fish ]
                |> GameArea.generate
            )
            ([ Animal.bear, Animal.wolf, Animal.deer, Animal.otter, Animal.fish, Animal.deer ]
                |> GameArea.generate
            )
        )
        >> (\( game, seed ) ->
                ( { game = game
                  , phase = Thinking { played = Nothing }
                  , error = Nothing
                  , useAutoTap = False
                  , seed = seed
                  }
                , Cmd.none
                )
           )


applyChange : Result String { game : Game, gamePhase : GamePhase } -> Model -> Model
applyChange result model =
    case result of
        Ok { gamePhase, game } ->
            { model
                | game = game
                , phase = gamePhase
                , error = Nothing
            }

        Err error ->
            { model | error = Just error }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedCard { id } ->
            ( case model.phase of
                WaitingForOpponent ->
                    model

                Thinking { played } ->
                    model
                        |> (if model.useAutoTap then
                                { gamePhase = model.phase, game = model.game }
                                    |> GamePhase.autoTap { id = id }
                                    |> applyChange

                            else
                                GamePhase.emptyMove
                                    { id = id
                                    , played = played |> Maybe.withDefault Set.empty
                                    , game = model.game
                                    }
                                    |> Maybe.map
                                        (\move ->
                                            { gamePhase = model.phase, game = model.game }
                                                |> GamePhase.tap move
                                                |> applyChange
                                        )
                                    |> Maybe.withDefault identity
                           )

                Tapping move ->
                    if move.animalId == id then
                        model

                    else
                        { model
                            | phase = Tapping (move |> Move.toggle id)
                        }

                Finished _ ->
                    model
            , Cmd.none
            )

        Canceled ->
            ( case model.phase of
                Tapping move ->
                    { model
                        | phase = Thinking { played = Just move.played }
                    }

                _ ->
                    model
            , Cmd.none
            )

        Confirmed ->
            ( model
                |> ({ gamePhase = model.phase, game = model.game }
                        |> GamePhase.end
                        |> Result.map
                            (\result ->
                                if result.gamePhase == WaitingForOpponent then
                                    { gamePhase = Thinking { played = Nothing }
                                    , game = result.game |> Game.swapAreas
                                    }

                                else
                                    result
                            )
                        |> applyChange
                   )
            , Cmd.none
            )

        PlayedCard index ->
            ( { gamePhase = model.phase, game = model.game }
                |> GamePhase.play index
                |> (\result ->
                        case result of
                            Ok { gamePhase, game } ->
                                { model
                                    | game = game
                                    , phase = gamePhase
                                    , error = Nothing
                                }

                            Err error ->
                                { model | error = Just error }
                   )
            , Cmd.none
            )

        ToggleAutoTap ->
            ( { model | useAutoTap = not model.useAutoTap }
            , Cmd.none
            )



--------------------------------------------------------------------------------


type alias Style msg =
    { dialog : DialogStyle msg
    , expansionPanel : ExpansionPanelStyle msg
    , button : ButtonStyle msg
    , primaryButton : ButtonStyle msg
    , tab : TabStyle msg
    , textInput : TextInputStyle msg
    , chipButton : ButtonStyle msg
    , row : RowStyle msg
    , buttonRow : RowStyle msg
    , column : ColumnStyle msg
    , cardColumn : ColumnStyle msg
    , selectButton : ButtonStyle msg
    , layout : LayoutStyle msg
    }


palette : Palette
palette =
    Material.defaultPalette


style : Style msg
style =
    { row = Material.row
    , buttonRow = Material.buttonRow
    , cardColumn = Material.cardColumn palette
    , column = Material.column
    , button = Material.outlinedButton palette
    , primaryButton = Material.containedButton palette
    , selectButton = Material.toggleButton palette
    , tab = Material.tab palette
    , textInput = Material.textInput palette
    , chipButton = Material.chip palette
    , expansionPanel = Material.expansionPanel palette
    , dialog = Material.alertDialog palette
    , layout = Material.layout palette
    }


viewArea :
    { gameArea : GameArea
    , animals : Dict Int Animal
    , phase : GamePhase
    , move : Maybe Move
    }
    -> Element Msg
viewArea { gameArea, animals, phase, move } =
    [ [ Element.text "Deck"
      , gameArea.deck
            |> List.map (.symbol >> Element.text)
            |> Widget.row style.row
      ]
        |> Element.column []
    , [ Element.text "Hand"
      , gameArea.hand
            |> Array.toList
            |> List.indexedMap
                (\index animal ->
                    View.squareCard
                        { color = Nothing
                        , text = animal.symbol
                        , header = ( animal.biome |> Animal.biomeToString, String.fromInt animal.strength )
                        , footer =
                            animal.behaviour
                                |> Animal.behaviourToString
                        , onPress =
                            case phase of
                                Thinking _ ->
                                    Just <| PlayedCard { index = index }

                                _ ->
                                    Nothing
                        }
                )
            |> Widget.row style.row
      ]
        |> Element.column []
    , [ Element.text "Battleground"
      , gameArea.placed
            |> Dict.toList
            |> List.map
                (\( id, { isTapped } ) ->
                    (case
                        animals
                            |> Dict.get id
                     of
                        Nothing ->
                            { color = Nothing
                            , text = "Id:" ++ String.fromInt id
                            , header =
                                ( "", "" )
                            , footer = ""
                            , onPress = Nothing
                            }

                        Just animal ->
                            case phase of
                                Tapping { selected, animalId } ->
                                    { color =
                                        if selected |> Set.member id then
                                            Just Color.blue

                                        else if animalId == id then
                                            Just Color.gray

                                        else
                                            Nothing
                                    , text = animal.symbol
                                    , header =
                                        ( animal.biome |> Animal.biomeToString
                                        , animal.strength |> String.fromInt
                                        )
                                    , footer =
                                        animal.behaviour
                                            |> Animal.behaviourToString
                                    , onPress = Just (ClickedCard { id = id })
                                    }

                                Thinking _ ->
                                    if isTapped then
                                        { color = Nothing
                                        , text =
                                            animals
                                                |> Dict.get id
                                                |> Maybe.map .symbol
                                                |> Maybe.withDefault ("Id:" ++ String.fromInt id)
                                        , header = ( animal.biome |> Animal.biomeToString, String.fromInt animal.strength )
                                        , footer =
                                            animal.behaviour
                                                |> Animal.behaviourToString
                                        , onPress = Nothing
                                        }

                                    else
                                        { color = Nothing
                                        , text =
                                            animals
                                                |> Dict.get id
                                                |> Maybe.map .symbol
                                                |> Maybe.withDefault ("Id:" ++ String.fromInt id)
                                        , header = ( animal.biome |> Animal.biomeToString, String.fromInt animal.strength )
                                        , footer =
                                            animal.behaviour
                                                |> Animal.behaviourToString
                                        , onPress = Just (ClickedCard { id = id })
                                        }

                                _ ->
                                    { color =
                                        case move of
                                            Just m ->
                                                if m.selected |> Set.member id then
                                                    Just Color.blue

                                                else
                                                    Nothing

                                            Nothing ->
                                                Nothing
                                    , text =
                                        animals
                                            |> Dict.get id
                                            |> Maybe.map .symbol
                                            |> Maybe.withDefault ("Id:" ++ String.fromInt id)
                                    , header = ( animal.biome |> Animal.biomeToString, String.fromInt animal.strength )
                                    , footer =
                                        animal.behaviour
                                            |> Animal.behaviourToString
                                    , onPress =
                                        if move == Nothing then
                                            Nothing

                                        else
                                            Just (ClickedCard { id = id })
                                    }
                    )
                        |> View.squareCard
                )
            |> Element.wrappedRow [ Element.spacing 10 ]
      ]
        |> Element.column []
    ]
        |> Widget.column style.cardColumn


{-| You can remove the msgMapper. But by doing so, make sure to also change `msg` to `Msg` in the line below.
-}
view : Model -> Element Msg
view model =
    let
        invalidRestrictions =
            case model.phase of
                Tapping move ->
                    case model.game |> Game.isValidMove move of
                        Ok () ->
                            []

                        Err list ->
                            list

                _ ->
                    []
    in
    [ [ [ Element.text "Your Area"
        , viewArea
            { gameArea = model.game.yourArea
            , animals = model.game.animals
            , phase =
                model.phase
            , move =
                case model.phase of
                    Tapping move ->
                        Just move

                    _ ->
                        Nothing
            }
        ]
            |> Widget.column style.column
      , [ Element.text "Opponent's Area"
        , viewArea
            { gameArea = model.game.oppArea
            , animals = model.game.animals
            , phase = WaitingForOpponent
            , move =
                case model.phase of
                    Tapping move ->
                        Just move

                    _ ->
                        Nothing
            }
        ]
            |> Widget.column style.column
      ]
        |> Element.wrappedRow [ Element.spacing 10, Element.width <| Element.shrink, Element.centerX ]
    , [ [ Widget.textButton style.button
            { text = "Cancel"
            , onPress = Just <| Canceled
            }
        , Widget.textButton style.primaryButton
            { text =
                case model.phase of
                    WaitingForOpponent ->
                        "Start Turn"

                    Thinking _ ->
                        "End Turn"

                    Tapping _ ->
                        "Confirm"

                    Finished True ->
                        "You Won"

                    Finished False ->
                        "You Lost"
            , onPress =
                case model.phase of
                    Tapping move ->
                        if invalidRestrictions |> List.isEmpty then
                            Just Confirmed

                        else
                            Nothing

                    _ ->
                        Just Confirmed
            }
        , model.error
            |> Maybe.withDefault ""
            |> Element.text
        ]
            |> Widget.row style.row
      , invalidRestrictions
            |> List.map Element.text
            |> Widget.column style.column
      , [ Input.checkbox []
            { onChange = always ToggleAutoTap
            , icon = Input.defaultCheckbox
            , checked = model.useAutoTap
            , label = Input.labelHidden "use Auto Tap"
            }
        , Element.text "Use Auto Tap"
        ]
            |> Widget.row style.row
      ]
        |> Element.row [ Element.centerX ]
    ]
        |> Element.column [ Element.width <| Element.fill ]


main : Program () Model Msg
main =
    Browser.element
        { init = always (init (Random.initialSeed 42))
        , view = view >> Element.layout []
        , update = update
        , subscriptions = always Sub.none
        }
