module Ecocards.Page.SinglePlayer exposing (Model)

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
import Element exposing (Element)
import Element.Input as Input
import Form.Decoder exposing (errors)
import Html.Attributes exposing (selected)
import List.Extra as List
import PixelEngine exposing (game)
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
    }


type Msg
    = ClickedCard { id : Int }
    | PlayedCard { index : Int }
    | Canceled
    | Confirmed
    | ToggleAutoTap


init : ( Model, Cmd Msg )
init =
    ( { game =
            { yourArea =
                { deck = [ Animal.wolf, Animal.fish, Animal.mouse ]
                , hand = [ Animal.cat, Animal.mouse, Animal.fish ] |> Array.fromList
                , placed = Dict.empty
                }
            , oppArea =
                { deck = [ Animal.bear, Animal.wolf, Animal.deer ]
                , hand = [ Animal.otter, Animal.fish, Animal.deer ] |> Array.fromList
                , placed = Dict.empty
                }
            , animals = Dict.empty
            , nextId = 1
            }
      , phase = Thinking { played = Nothing }
      , error = Nothing
      , useAutoTap = True
      }
    , Cmd.none
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
        |> Widget.row style.row
    , [ Element.text "Hand"
      , gameArea.hand
            |> Array.toList
            |> List.indexedMap
                (\index animal ->
                    Widget.button style.chipButton
                        { text = animal.symbol
                        , icon = Element.none
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
        |> Widget.row style.row
    , [ Element.text "Animals"
      , gameArea.placed
            |> Dict.toList
            |> List.map
                (\( id, { isTapped } ) ->
                    case phase of
                        Tapping { selected } ->
                            ( selected |> Set.member id
                            , { text =
                                    animals
                                        |> Dict.get id
                                        |> Maybe.map .symbol
                                        |> Maybe.withDefault ("Id:" ++ String.fromInt id)
                              , icon = Element.none
                              , onPress = Just (ClickedCard { id = id })
                              }
                            )

                        Thinking _ ->
                            if isTapped then
                                ( False
                                , { text =
                                        animals
                                            |> Dict.get id
                                            |> Maybe.map .symbol
                                            |> Maybe.withDefault ("Id:" ++ String.fromInt id)
                                  , icon = Element.none
                                  , onPress = Nothing
                                  }
                                )

                            else
                                ( False
                                , { text =
                                        animals
                                            |> Dict.get id
                                            |> Maybe.map .symbol
                                            |> Maybe.withDefault ("Id:" ++ String.fromInt id)
                                  , icon = Element.none
                                  , onPress = Just (ClickedCard { id = id })
                                  }
                                )

                        _ ->
                            ( case move of
                                Just m ->
                                    m.selected |> Set.member id

                                Nothing ->
                                    False
                            , { text =
                                    animals
                                        |> Dict.get id
                                        |> Maybe.map .symbol
                                        |> Maybe.withDefault ("Id:" ++ String.fromInt id)
                              , icon = Element.none
                              , onPress =
                                    if move == Nothing then
                                        Nothing

                                    else
                                        Just (ClickedCard { id = id })
                              }
                            )
                )
            |> Widget.buttonRow
                { list = style.buttonRow
                , button = style.chipButton
                }
      ]
        |> Widget.row style.row
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
        |> Element.wrappedRow [ Element.spaceEvenly, Element.width <| Element.fill ]
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
        |> Widget.row style.row
    , [ "animals: " ++ Debug.toString model.game.animals |> Element.text |> List.singleton |> Element.paragraph []
      , "yourArea: " ++ Debug.toString model.game.yourArea |> Element.text |> List.singleton |> Element.paragraph []
      , "oppArea: " ++ Debug.toString model.game.oppArea |> Element.text |> List.singleton |> Element.paragraph []
      , "phase: " ++ Debug.toString model.phase |> Element.text |> List.singleton |> Element.paragraph []
      ]
        |> Element.column [ Element.width <| Element.fill ]
    ]
        |> Element.column [ Element.width <| Element.fill ]


main : Program () Model Msg
main =
    Browser.element
        { init = always init
        , view = view >> Element.layout []
        , update = update
        , subscriptions = always Sub.none
        }
