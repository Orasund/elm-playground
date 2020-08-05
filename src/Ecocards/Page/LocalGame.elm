module Ecocards.Page.LocalGame exposing (Model, Msg, init, subscriptions, update, view)

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
import Element.Font as Font
import Element.Input as Input
import Form.Decoder exposing (errors)
import Html exposing (Html)
import Html.Attributes exposing (selected)
import List.Extra as List
import PixelEngine exposing (game)
import Random exposing (Seed)
import Set exposing (Set)
import Set.Extra as Set
import Time
import Widget
import Widget.Snackbar as Snackbar exposing (Snackbar)
import Widget.Style exposing (ButtonStyle, ColumnStyle, DialogStyle, ExpansionPanelStyle, LayoutStyle, RowStyle, SnackbarStyle, SortTableStyle, TabStyle, TextInputStyle)
import Widget.Style.Material as Material exposing (Palette)


type alias Model =
    { game : Game
    , phase : GamePhase
    , error : Maybe String
    , useAutoTap : Bool
    , seed : Seed
    , snackbar : Snackbar { title : String, desc : String }
    , showDialog : Bool
    , useAutoPlay : Bool
    , useAutoOpp : Bool
    }


type Msg
    = ClickedCard { id : Int }
    | PlayedCard { index : Int }
    | Canceled
    | Confirmed
    | ToggleAutoTap
    | ToggleAutoPlay
    | ToggleAutoOpp
    | GetInfo { title : String, desc : String }
    | TimePassed Int
    | CloseDialog Bool
    | AutoPlay


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
                  , useAutoPlay = False
                  , useAutoOpp = False
                  , seed = seed
                  , snackbar = Snackbar.init
                  , showDialog = False
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
                                                |> (case model.game.animals |> Dict.get id of
                                                        Just { behaviour } ->
                                                            case behaviour of
                                                                Herbivores _ ->
                                                                    Result.andThen GamePhase.end

                                                                _ ->
                                                                    identity

                                                        Nothing ->
                                                            identity
                                                   )
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
            case model.phase of
                Finished _ ->
                    init model.seed

                Thinking _ ->
                    ( if
                        model.game.yourArea.placed
                            |> Dict.filter (\id { isTapped } -> not isTapped)
                            |> Dict.isEmpty
                      then
                        { gamePhase = model.phase, game = model.game }
                            |> GamePhase.end
                            |> (\result ->
                                    case result of
                                        Ok ok ->
                                            if ok.gamePhase == WaitingForOpponent then
                                                model
                                                    |> applyChange
                                                        (Ok
                                                            { gamePhase = Thinking { played = Nothing }
                                                            , game = ok.game |> Game.swapAreas
                                                            }
                                                        )
                                                    |> (\m ->
                                                            { m
                                                                | useAutoPlay =
                                                                    model.useAutoPlay
                                                                        |> (if model.useAutoOpp then
                                                                                not

                                                                            else
                                                                                identity
                                                                           )
                                                            }
                                                       )

                                            else
                                                model |> applyChange (Ok ok)

                                        Err err ->
                                            model |> applyChange (Err err)
                               )

                      else
                        { model | showDialog = True }
                    , Cmd.none
                    )

                _ ->
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

        ToggleAutoPlay ->
            ( { model | useAutoPlay = not model.useAutoPlay }
            , Cmd.none
            )

        ToggleAutoOpp ->
            ( { model | useAutoOpp = not model.useAutoOpp }
            , Cmd.none
            )

        GetInfo behaviour ->
            ( { model | snackbar = model.snackbar |> Snackbar.insert behaviour }
            , Cmd.none
            )

        TimePassed int ->
            ( { model | snackbar = model.snackbar |> Snackbar.timePassed int }
            , Cmd.none
            )

        CloseDialog shouldContinue ->
            if shouldContinue then
                ( { gamePhase = model.phase, game = model.game }
                    |> GamePhase.end
                    |> (\result ->
                            case result of
                                Ok ok ->
                                    if ok.gamePhase == WaitingForOpponent then
                                        model
                                            |> applyChange
                                                (Ok
                                                    { gamePhase = Thinking { played = Nothing }
                                                    , game = ok.game |> Game.swapAreas
                                                    }
                                                )
                                            |> (\m ->
                                                    { m
                                                        | useAutoPlay =
                                                            model.useAutoPlay
                                                                |> (if model.useAutoOpp then
                                                                        not

                                                                    else
                                                                        identity
                                                                   )
                                                    }
                                               )

                                    else
                                        model |> applyChange (Ok ok)

                                Err err ->
                                    model |> applyChange (Err err)
                       )
                    |> (\m -> { m | showDialog = False })
                , Cmd.none
                )

            else
                ( { model | showDialog = False }, Cmd.none )

        AutoPlay ->
            ( { gamePhase = model.phase, game = model.game }
                |> GamePhase.autoPlay
                |> (\result ->
                        case result of
                            Ok ok ->
                                if ok.gamePhase == WaitingForOpponent then
                                    model
                                        |> applyChange
                                            (Ok
                                                { gamePhase = Thinking { played = Nothing }
                                                , game = ok.game |> Game.swapAreas
                                                }
                                            )
                                        |> (\m ->
                                                { m
                                                    | useAutoPlay =
                                                        model.useAutoPlay
                                                            |> (if model.useAutoOpp then
                                                                    not

                                                                else
                                                                    identity
                                                               )
                                                }
                                           )

                                else
                                    model |> applyChange (Ok ok)

                            Err err ->
                                model |> applyChange (Err err)
                   )
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every 50 (always (TimePassed 50))
        , if model.useAutoPlay then
            Time.every 700 (always AutoPlay)

          else
            Sub.none
        ]



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
    , snackbar : SnackbarStyle msg
    , textButton : ButtonStyle msg
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
    , snackbar = Material.snackbar palette
    , textButton = Material.textButton palette
    }


viewArea :
    { gameArea : GameArea
    , animals : Dict Int Animal
    , phase : GamePhase
    , move : Maybe Move
    }
    -> Element Msg
viewArea { gameArea, animals, phase, move } =
    [ [ [ Element.text "Next Cards "
        , gameArea.deck
            |> List.take (3 - (gameArea.hand |> Array.length))
            |> List.map (.symbol >> Element.text)
            |> Element.row [ Element.alignRight, Element.spacing 8 ]
        ]
            |> Element.column [ Element.alignTop, Element.spacing 8 ]
      , [ Element.text "Deck"
        , gameArea.deck
            |> List.drop (3 - (gameArea.hand |> Array.length))
            |> List.map (.symbol >> Element.text)
            |> Element.row [ Element.spacing 8 ]
        ]
            |> Element.column [ Element.spacing 8 ]
      ]
        |> Element.row [ Element.spacing 10 ]
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
                        , getInfoMsg =
                            Just <| GetInfo <| Animal.behaviourDescription <| animal.behaviour
                        }
                )
            |> Widget.row style.row
      ]
        |> Element.column []
    , [ Element.text "Battle Area"
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
                            , getInfoMsg =
                                Nothing
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
                                    , getInfoMsg =
                                        Just <| GetInfo <| Animal.behaviourDescription <| animal.behaviour
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
                                        , getInfoMsg =
                                            Just <| GetInfo <| Animal.behaviourDescription <| animal.behaviour
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
                                        , getInfoMsg =
                                            Just <| GetInfo <| Animal.behaviourDescription <| animal.behaviour
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
                                    , getInfoMsg =
                                        Just <| GetInfo <| Animal.behaviourDescription <| animal.behaviour
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
view : Model -> Html Msg
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
            |> Element.column [ Element.spacing 8, Element.alignTop ]
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
            |> Element.column [ Element.spacing 8, Element.alignTop ]
      ]
        |> Element.wrappedRow [ Element.spacing 10, Element.width <| Element.shrink, Element.centerX ]
    , (case model.phase of
        WaitingForOpponent ->
            "Wait for opponent players"

        Thinking _ ->
            "Play cards from your hand, tap cards on the battle area or end your turn"

        Tapping _ ->
            "Choose animals to remove before tapping your card"

        Finished True ->
            "You Won"

        Finished False ->
            "You Lost"
      )
        |> Element.text
        |> Element.el [ Element.centerX ]
    , [ [ Widget.textButton style.button
            { text = "Cancel"
            , onPress =
                case model.phase of
                    WaitingForOpponent ->
                        Nothing

                    Thinking _ ->
                        Nothing

                    Tapping _ ->
                        Just <| Canceled

                    Finished True ->
                        Nothing

                    Finished False ->
                        Nothing
            }
        , Widget.textButton style.primaryButton
            { text =
                case model.phase of
                    WaitingForOpponent ->
                        "Start Turn"

                    Thinking _ ->
                        "End Turn"

                    Tapping _ ->
                        "Tap Card"

                    Finished _ ->
                        "Replay"
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
        , Widget.textButton style.textButton
            { text = "Help"
            , onPress =
                case model.phase of
                    WaitingForOpponent ->
                        Nothing

                    Thinking _ ->
                        { title = "Your Turn"
                        , desc = "Play a card from your hand by clicking on it. Tap a played card on the battle area by clicking on it. You may end your turn once you have played at least one card. Once your turn is over ALL CARDS THAT HAVE NOT BEEN TAPPED GET REMOVED!"
                        }
                            |> GetInfo
                            |> Just

                    Tapping _ ->
                        { title = "Tapping Action"
                        , desc = "Select a set of animals that should be removed. For more detail, checkout the info of the selected card."
                        }
                            |> GetInfo
                            |> Just

                    Finished True ->
                        Nothing

                    Finished False ->
                        Nothing
            }
        ]
            |> Widget.row style.row
      ]
        |> Element.row [ Element.centerX ]
    , [ [ Input.checkbox []
            { onChange = always ToggleAutoTap
            , icon = Input.defaultCheckbox
            , checked = model.useAutoTap
            , label = Input.labelHidden "Use Auto Tap"
            }
        , Element.text "Use Auto Tap"
        ]
            |> Widget.row style.row
      , [ Input.checkbox []
            { onChange = always ToggleAutoPlay
            , icon = Input.defaultCheckbox
            , checked = model.useAutoPlay
            , label = Input.labelHidden "Use Auto Play"
            }
        , Element.text "Use Auto Play"
        ]
            |> Widget.row style.row
      , [ Input.checkbox []
            { onChange = always ToggleAutoOpp
            , icon = Input.defaultCheckbox
            , checked = model.useAutoOpp
            , label = Input.labelHidden "Computer Opponent"
            }
        , Element.text "Computer Opponent"
        ]
            |> Widget.row style.row
      ]
        |> Element.row [ Element.spacing 10, Element.centerX ]
    , model.error
        |> Maybe.withDefault ""
        |> Element.text
    , invalidRestrictions
        |> List.map Element.text
        |> Element.column [ Element.centerX ]
    ]
        |> Element.column [ Element.width <| Element.fill ]
        |> Element.layout
            ([ Snackbar.view style.snackbar
                (\{ title, desc } ->
                    { text = desc
                    , button = Nothing
                    }
                )
                model.snackbar
                |> Maybe.withDefault Element.none
                |> Element.el
                    [ Element.alignBottom
                    , Element.alignRight
                    , Element.padding 5
                    , Font.size 14
                    ]
                |> Element.inFront
             ]
                ++ (if model.showDialog then
                        Widget.dialog style.dialog
                            { title = Just "Untapped Animals"
                            , text = "Some of the animals in your battle area have not been tapped. If you continue these animals will die."
                            , accept =
                                Just
                                    { text = "Continue"
                                    , onPress = Just <| CloseDialog True
                                    }
                            , dismiss =
                                Just
                                    { text = "Cancel"
                                    , onPress = Just <| CloseDialog False
                                    }
                            }

                    else
                        []
                   )
            )


main : Program () Model Msg
main =
    Browser.element
        { init = always (init (Random.initialSeed 42))
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }
