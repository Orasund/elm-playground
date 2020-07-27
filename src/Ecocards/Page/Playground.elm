module Ecocards.Page.Playground exposing (Model)

import Array
import Browser
import Dict exposing (Dict)
import Ecocards.Data.Animal as Animal exposing (Animal, Behaviour(..))
import Ecocards.Data.Game as Game exposing (Game)
import Ecocards.Data.GameArea as GameArea exposing (GameArea)
import Ecocards.Data.GamePhase as GamePhase exposing (GamePhase(..))
import Ecocards.Data.Move as Move exposing (Move)
import Element exposing (Element)
import Html.Attributes exposing (selected)
import Set exposing (Set)
import Set.Extra as Set
import Widget
import Widget.Style exposing (ButtonStyle, ColumnStyle, DialogStyle, ExpansionPanelStyle, LayoutStyle, RowStyle, SortTableStyle, TabStyle, TextInputStyle)
import Widget.Style.Material as Material exposing (Palette)


type alias Model =
    { game : Game
    , phase : GamePhase
    , error : Maybe String
    }


type Msg
    = ClickedCard { id : Int }
    | PlayedCard { index : Int }
    | Canceled
    | Confirmed


init : ( Model, Cmd Msg )
init =
    ( { game =
            { yourArea =
                { deck = [ Animal.fish, Animal.fish, Animal.fish ]
                , hand = [ Animal.cat, Animal.fish, Animal.fish ] |> Array.fromList
                , placed = Dict.empty
                }
            , oppArea =
                { deck = [ Animal.fish, Animal.fish, Animal.fish ]
                , hand = [ Animal.fish, Animal.fish, Animal.fish ] |> Array.fromList
                , placed = Dict.empty
                }
            , animals = Dict.empty
            , nextId = 1
            }
      , phase = Thinking { played = Set.empty }
      , error = Nothing
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedCard { id } ->
            ( model.game.animals
                |> Dict.get id
                |> Maybe.map
                    (\animal ->
                        case model.phase of
                            WaitingForOpponent ->
                                model

                            Thinking { played } ->
                                if model.game.yourArea.placed |> Dict.member id then
                                    { model
                                        | phase =
                                            let
                                                ( minAnimal, maxAnimal ) =
                                                    case animal.behaviour of
                                                        Predator _ amounts ->
                                                            amounts

                                                        Herbivores _ ->
                                                            ( 0, 0 )

                                                        Omnivorous amounts ->
                                                            amounts
                                            in
                                            Tapping
                                                { card = id
                                                , selected = Set.empty
                                                , played = played
                                                , minAmount = minAnimal
                                                , maxAmount = maxAnimal
                                                }
                                    }

                                else
                                    model

                            Tapping move ->
                                if move.card == id then
                                    model

                                else
                                    { model
                                        | phase = Tapping (move |> Move.toggle id)
                                    }

                            Finished _ ->
                                model
                    )
                |> Maybe.withDefault model
            , Cmd.none
            )

        Canceled ->
            ( case model.phase of
                Tapping move ->
                    { model
                        | phase = Thinking { played = move.played }
                    }

                _ ->
                    model
            , Cmd.none
            )

        Confirmed ->
            ( case GamePhase.end ( model.phase, model.game ) of
                Ok ( phase, game ) ->
                    if phase == WaitingForOpponent then
                        { model
                            | phase = Thinking { played = Set.empty }
                            , game = game |> Game.swapAreas
                            , error = Nothing
                        }

                    else
                        { model
                            | phase = phase
                            , game = game
                            , error = Nothing
                        }

                Err error ->
                    { model | error = Just error }
            , Cmd.none
            )

        PlayedCard { index } ->
            case model.phase of
                Thinking { played } ->
                    ( case model.game |> Game.play { index = index } of
                        Ok game ->
                            { model
                                | game = game
                                , phase = Thinking { played = played |> Set.insert (game.nextId - 1) }
                                , error = Nothing
                            }

                        Err error ->
                            { model | error = Just error }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )



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
            |> Element.wrappedRow []
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
            |> Element.wrappedRow []
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
        |> Widget.row style.row
    , [ Widget.textButton style.button
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
    , Debug.toString model.game |> Element.text |> List.singleton |> Element.paragraph []
    , Debug.toString model.phase |> Element.text |> List.singleton |> Element.paragraph []
    ]
        |> Widget.column style.column


main : Program () Model Msg
main =
    Browser.element
        { init = always init
        , view = view >> Element.layout []
        , update = update
        , subscriptions = always Sub.none
        }
